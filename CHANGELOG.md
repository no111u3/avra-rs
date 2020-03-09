## Release 0.2 (2020-03-09)

First works release of AVRA-RS

- Update project targets and description
- Add Ifndef/Ifdef/If/ElIf/Else conditional directive support
- Add Define directive support with store in preprocessor context
- Add conditional rules for expressions
- Add include/include path and include file support
- Add standard include directive support
- Add Def/Undef directive support for register aliases
- Add Set directive support for redefined equs
- Add Macro/EndMacro/EndM directive support and macro parsing
- Add full macro support with arguments, recursive and nested macro
- Add write output hex for eeprom data (with customize name by `-e` key)
- Add case insensitive support for defs/sets/equs and labels
- Add support of basic and extended C comments
- Add support for sbic/sbis/sbrc/sbrs/bst/bld instructions
- Add draft Pragma directive support
- Fix in/out instruction opcodes generation
- Fix hex writer for compatibility with common standard

## Release 0.1 (2020-02-29)

Start implementation of AVRA-RS

- Added support build from file with output as hex
- Added includes support with relative paths
- Added first version of AVRA-RS
- Added first version of avra_lib with fully interface for parse and build assembler files