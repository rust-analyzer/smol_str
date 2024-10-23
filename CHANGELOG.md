# Changelog

## 0.3.2 - 2024-10-23

- Fix `SmolStrBuilder::push` incorrectly padding null bytes when spilling onto the heap on a
  multibyte character push

## 0.3.1 - 2024-09-04

- Fix `SmolStrBuilder` leaking implementation details

## 0.3.0 - 2024-09-04

- Remove deprecated `SmolStr::new_inline_from_ascii` function
- Remove `SmolStr::to_string` in favor of `ToString::to_string`
- Add `impl AsRef<[u8]> for SmolStr` impl
- Add `impl AsRef<OsStr> for SmolStr` impl
- Add `impl AsRef<Path> for SmolStr` impl
- Add `SmolStrBuilder`

## 0.2.2 - 2024-05-14

- Add `StrExt` trait providing `to_lowercase_smolstr`, `replace_smolstr` and similar
- Add `PartialEq` optimization for `ptr_eq`-able representations
