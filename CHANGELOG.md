### 0.2.0.2 -- 2024-03-15

* Compatibility with [MicroHs](https://github.com/augustss/MicroHs)
* Performance improvements
* Fix `compileBounded`'s behavior on negative bounds

### 0.2.0.1 -- 2024-12-25

* Documentation improvements

### 0.2.0.0 -- 2024-11-24

* Breaking changes
  * Parsing fails more eagerly. This affects lazy list parsing and parsing via
    the `Regex.Base` functions `prepareParser` and `stepParser`.
* Additions
  * Added `Regex.Base.parseNext`.
* Other
  * Some internal modules are now exported.

### 0.1.0.0 -- 2024-03-04

* First version.
