# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Calendar Versioning](https://calver.org/) (`YYYY.M.D.N`).

## [Unreleased]

### Changed
- Cleaned up dead code in lint rules (`check-inappropriate-equality`, `find-unhygienic-macro-bindings`)
- Simplified `print-sexp` tag dispatch to use `collection-tag-p` helper
- Optimized worker queue insertion from O(n) `append` to O(1) `nconc`
- Removed unused `strategy` parameter from `merge-workspaces`
- Simplified `copy-file-between-workspaces` to avoid unnecessary `multiple-value-bind`
- Updated AGENTS.md file map to reflect actual module structure

### Documented
- Added limitation note to `merge-file-ast` docstring about positional merge behavior

## [2026.9.28.1] - 2026-09-28

### Changed
- Updated readme.md

## [2026.9.28] - 2026-09-28

### Fixed
- CalVer script computes sequential daily versions and cleans stdout

### Changed
- Applied idiomatic `when` usage and curated redundant comments across analysis and MCP modules
- Decomposed `analysis.lisp` and `mcp.lisp` into submodules
- Streamlined `define-mcp-tool` handlers, configured formatter templates, and compressed codebase (-842 LOC)
- Implemented CLOS protocols for linter rules and dialect formatting
- Introduced `define-mcp-tool` macro and unified tool registration
- Replaced chained `cond` string comparisons with `trivia:match`
- Applied structural AST optimizations and decomposed worker loop

### Added
- 10 multi-dialect static analysis rules with dynamic context
- Code comment and docstring value assessment to quality swarm
- Jev System 1 skills and workflow directives
- Skeleton mode, `read_slice`, and overwrite safeguard for wide ASTs
- In-memory file creation, 3-way AST merge, rebasing, and pagination
- Workspace status, diff, merge, and selective commit (Phase 3)
- Workspace lifecycle and management tools (Phase 2)
- Workspace registry and protocol routing (Phase 1)
- Optimistic Concurrency Control and worker pool for multi-agent support

### Fixed
- MCP uses `string-case` for JSON-RPC method and action dispatch
- Format script only reports and overwrites files modified by formatter
- Skills enforce structural-editing MCP tools and real `invoke_subagent` swarm execution
- Workspace/refactor: deadlock-free multi-workspace locking and dialect-aware refactoring templates
- CI resolves Quicklisp missing dependencies in release workflow

## [2026.9.25.4] - 2026-09-25

### Changed
- Structural AST optimizations, worker loop decomposition

## [2026.9.25.3] - 2026-09-25

### Changed
- Version bump

## [2026.9.25.2] - 2026-09-25

### Added
- Skeleton mode for wide AST nodes
- `read_slice` vertical ray path inspection
- Overwrite safeguard for nodes with >50 children

## [2026.9.25.1] - 2026-09-25

### Fixed
- CI Quicklisp dependency resolution in release workflow

## [2026.9.21.5] - 2026-09-21

### Changed
- CalVer release bump

## [2026.9.18.57] - 2026-09-18

### Changed
- Improved formatter with cl-indentify integration

## [2026.9.18] - 2026-09-18

### Added
- Initial public release
- 22 MCP tools for AST-level structural editing
- Multi-dialect support: Common Lisp, Clojure, Scheme, Emacs Lisp, Fennel
- Multi-branch in-memory workspace staging with fork/merge/rebase
- 19 structural lint rules
- Cyclomatic complexity and nesting depth metrics
- Duplicate subtree detection via Merkle hashing
- Lexical variable binding analysis
- Unified refactoring advisor
- Standalone binaries for Linux, macOS, Windows
- Multi-arch Docker container images
