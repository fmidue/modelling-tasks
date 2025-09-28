# Scripts

This directory contains validation scripts for the repository.

## EditorConfig Checker Script (`check-editorconfig.sh`)

This script validates that all files in the repository comply with `.editorconfig` settings.

### Purpose

Ensures that:

1. No files have trailing whitespace (except test/unit/\*\* files)
2. All files end with a final newline (except test/unit/\*\* files)
3. Files use LF line endings (except test/unit/\*\* files)
4. Follows the `.editorconfig` rules defined in the repository

### Usage

```bash
./scripts/check-editorconfig.sh
```

### What it checks

- **Trailing whitespace**: Looks for lines ending with spaces or tabs
- **Final newlines**: Ensures non-empty files end with a newline character
- **Line endings**: Checks for CRLF vs LF line endings
- **File patterns**: Applies different rules based on `.editorconfig` patterns

### Exit codes

- 0: All files comply with .editorconfig
- 1: Violations found (fails CI)

## IO Checker Script (`check-no-runaway-io.sh`)

This script checks for runaway IO usage in the library code (src/ folder).

### Purpose

After issue #242 introduced capabilities for various IO computations, this
checker ensures that:

1. No direct System.IO imports are used in src/ folder
2. No direct IO functions (readFile, writeFile, putStr, etc.) are used in
   src/ folder
3. Only Capabilities.\* modules are used for IO operations
4. The library maintains proper separation of concerns

### IO Checker Usage

```bash
./scripts/check-no-runaway-io.sh
```

### IO Checker Rules

#### Prohibited patterns

- `import System.IO`
- `import qualified System.IO`
- Direct IO functions: `readFile`, `writeFile`, `putStr`, `putStrLn`,
  `getLine`, etc.
- Unsafe IO: `unsafePerformIO`
- Debug functions: `Debug.Trace`

#### Allowed patterns

- `import Capabilities.*` modules
- `import qualified Capabilities.*` modules
- MonadRandom functions like `shuffleM`
- Comments containing IO patterns

### Integration

This checker is integrated into the CI pipeline via
`.github/workflows/checks.yml` and runs on every push and pull request.

### IO Checker Exit codes

- 0: No violations found
- 1: Violations found (fails CI)
