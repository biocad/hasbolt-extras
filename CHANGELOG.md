# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.0.0.8] - 2018-05-14
### Added
- Added `DSL` for `Cypher`.

## [0.0.0.7] - 2018-04-23
### Added
- Added ability to delete nodes by their `BoltId`s.

## [0.0.0.6] - 2018-04-20
### Added
- Added ability to update properties of the existing node; added ability to choose
if you want to `CREATE` or `MERGE` the relationship.

## [0.0.0.4] - 2018-04-05
### Changed
- More accurate `toNode` on data fields with `Maybe a` type. If the corresponding field in the type is Nothing, this field won't be included to `Node`.

## [0.0.0.3] - 2018-04-05
### Changed
- More accurate `fromNode` on data fields with `Maybe a` type. If there is no such field in Node, data field will be set to Nothing.

## [0.0.0.0] - 2018-02-22
### Added
- Template Haskell code to generate `Node`s and `URelationship`s.
- Simple queries to upload `Node` and `URelationship`.
- Simple query to download `Node`s.
