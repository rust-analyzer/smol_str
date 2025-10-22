use std::{
    env, fs,
    process::{self, Command, ExitStatus, Stdio},
    time::Instant,
};

type Error = Box<dyn std::error::Error>;
type Result<T> = std::result::Result<T, Error>;

fn main() {
    if let Err(err) = try_main() {
        eprintln!("{}", err);
        process::exit(1);
    }
}

fn try_main() -> Result<()> {
    let cwd = env::current_dir()?;
    let cargo_toml = cwd.join("Cargo.toml");
    assert!(
        cargo_toml.exists(),
        "Cargo.toml not found, cwd: {}",
        cwd.display()
    );

    {
        let _s = Section::new("BUILD_NO_DEFAULT_FEATURES");
        shell("cargo test --all-features --workspace --no-run --no-default-features")?;
    }

    {
        let _s = Section::new("BUILD");
        shell("cargo test --all-features --workspace --no-run")?;
    }

    {
        let _s = Section::new("TEST");
        shell("cargo test --all-features --workspace")?;
        shell("cargo test --no-default-features --workspace")?;
    }

    {
        let _s = Section::new("TEST_BENCHES");
        shell("cargo test --benches --all-features")?;
    }

    let current_branch = shell_output("git branch --show-current")?;
    if &current_branch == "master" {
        let _s = Section::new("PUBLISH");
        let manifest = fs::read_to_string(&cargo_toml)?;
        let version = get_field(&manifest, "version")?;
        let tag = format!("v{}", version);
        let tags = shell_output("git tag --list")?;

        if !tags.contains(&tag) {
            let token = env::var("CRATES_IO_TOKEN").unwrap();
            shell(&format!("git tag v{}", version))?;
            shell(&format!("cargo publish --token {}", token))?;
            shell("git push --tags")?;
        }
    }
    Ok(())
}

fn get_field<'a>(text: &'a str, name: &str) -> Result<&'a str> {
    for line in text.lines() {
        let words = line.split_ascii_whitespace().collect::<Vec<_>>();
        match words.as_slice() {
            [n, "=", v, ..] if n.trim() == name => {
                assert!(v.starts_with('"') && v.ends_with('"'));
                return Ok(&v[1..v.len() - 1]);
            }
            _ => (),
        }
    }
    Err(format!("can't find `{}` in\n----\n{}\n----\n", name, text))?
}

fn shell(cmd: &str) -> Result<()> {
    let status = command(cmd).status()?;
    check_status(status)
}

fn shell_output(cmd: &str) -> Result<String> {
    let output = command(cmd).stderr(Stdio::inherit()).output()?;
    check_status(output.status)?;
    let res = String::from_utf8(output.stdout)?;
    let res = res.trim().to_string();
    println!("{}", res);
    Ok(res)
}

fn command(cmd: &str) -> Command {
    eprintln!("> {}", cmd);
    let words = cmd.split_ascii_whitespace().collect::<Vec<_>>();
    let (cmd, args) = words.split_first().unwrap();
    let mut res = Command::new(cmd);
    res.args(args);
    res
}

fn check_status(status: ExitStatus) -> Result<()> {
    if !status.success() {
        Err(format!("$status: {}", status))?;
    }
    Ok(())
}

struct Section {
    name: &'static str,
    start: Instant,
}

impl Section {
    fn new(name: &'static str) -> Section {
        println!("::group::{}", name);
        let start = Instant::now();
        Section { name, start }
    }
}

impl Drop for Section {
    fn drop(&mut self) {
        eprintln!("{}: {:.2?}", self.name, self.start.elapsed());
        println!("::endgroup::");
    }
}
