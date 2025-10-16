# Modelling Tasks

Modelling Tasks is a Haskell library and application suite for generating exercise tasks for modelling lecture contents. It covers UML Activity Diagrams, Class Diagrams, Object Diagrams, and Petri nets.

**Always reference these instructions first and fall back to search or Bash commands only when you encounter unexpected information that does not match the info here.**

## 🤖 Automated Copilot Setup

This repository includes an automated setup workflow (`.github/workflows/copilot-setup-steps.yml`) that prepares the environment for Copilot operations. The workflow automatically:

- **Installs system dependencies**: graphviz, texlive-base, texlive-latex-base
- **Verifies installations**: Checks Graphviz and LaTeX are working
- **Sets up Haskell Stack**: Configures the build environment
- **Installs HLint**: Enables code quality checking
- **Pre-builds dependencies**: Downloads and builds Haskell project dependencies

System dependencies and Haskell dependencies are already available. You can immediately proceed with builds and tests.

## 🚨 CRITICAL WARNINGS

### 🔴 NEVER COMMIT FILES THAT VIOLATE .EDITORCONFIG

**ABSOLUTE REQUIREMENT**: Every file you create or modify MUST comply with `.editorconfig` rules:

- **NO TRAILING WHITESPACE**
- **FINAL NEWLINE REQUIRED** (except test/unit/\*\* files)

**BEFORE ANY COMMIT**: Run `./scripts/check-editorconfig.sh` to validate compliance:

```bash
./scripts/check-editorconfig.sh
```

**If violations found**: Use these commands to fix them immediately:

```bash
# For individual files:
sed -i 's/[[:space:]]*$//' filename  # Remove trailing whitespace
echo >> filename                     # Add final newline
```

Then run EditorConfig validation again to confirm fixes:

```bash
./scripts/check-editorconfig.sh
```

**IF `./scripts/check-editorconfig.sh` FAILS**:

- **DO NOT COMMIT**
- **DO NOT USE `report_progress`**
- **FIX ALL VIOLATIONS FIRST**

### ⏰ NEVER CANCEL BUILDS OR TESTS

- **Project builds**: 30-45 minutes (set timeout to 60+ minutes)
- **Test suites**: 15-30 minutes (set timeout to 45+ minutes)
- Builds resume from cache when interrupted properly - canceling wastes progress

## Working Effectively

### Build Tool Setup

This project uses Haskell Stack as its primary build tool. Three Stack configurations are available:

- `stack.yaml` -- main library configuration
- `stack-apps.yaml` -- applications configuration (includes app/, legacy-app/, example/)
- `stack-examples.yaml` -- examples only configuration (includes example/)

### Building the Project

**NEVER CANCEL builds or dependency installations - they can take 60+ minutes**

Dependencies are pre-installed by the automated setup workflow. Build the project with:

- `stack --stack-yaml=stack-apps.yaml build` -- Builds the main library plus all applications in `/app`, `/legacy-app`, and `/example` (30-45 minutes)
- `stack build` -- Build library only (15-30 minutes)
- `stack --stack-yaml=stack-examples.yaml build` -- Build examples (15-30 minutes)

### Running Tests

- `stack test` -- Takes 15-30 minutes. Set timeout to 45+ minutes.
- `stack --stack-yaml=stack-apps.yaml test` -- includes all test suites
- Test-specific options:
  - `--test-arguments="--skip-needs-tuning"` -- excludes unstable/long-running tests
  - `--test-arguments="--times --maximum-generated-tests=50"` -- limits test case generation

**NEVER CANCEL**: Allow adequate time for tests to complete.

### Running Applications

The project includes multiple command-line applications in the `/app` directory:

- `stack exec match-cd-od` -- Match class and object diagrams
- `stack exec different-names` -- Generate different name variations
- `stack exec repair-incorrect` -- Repair incorrect models
- `stack exec check-cds` -- Check class diagrams
- `stack exec concurrency` -- Concurrency analysis
- `stack exec conflicts` -- Conflict analysis

### Using GHCi for Interactive Development

The repository includes a `.ghci` configuration file with preloaded modules and settings:

```haskell
-- .ghci automatically loads:
-- :set +s (show timing)
-- :set -XTypeApplications
-- :set -iapp/common (include path)
-- :l app/common/Common.hs (loads Common module)
-- Pre-imported: Control.OutputCapable.Blocks, Control.Monad.Trans.Except
-- Qualified imports: Data.Bimap as BM, Data.Map as M
```

For interactive task generation and testing:

```bash
stack ghci --stack-yaml=stack-examples.yaml
```

Example GHCi session for NameCdError task:

```haskell
:m + Capabilities.Alloy.IO Capabilities.Cache.IO Capabilities.Diagrams.IO Capabilities.Graphviz.IO Capabilities.PlantUml.IO
:m + Control.OutputCapable.Blocks Control.OutputCapable.Blocks.Generic
inst <- nameCdErrorGenerate defaultNameCdErrorConfig 0 0
runLangMReport (return ()) (>>) (nameCdErrorTask "/tmp/" inst) >>= \(Just (), x) -> (x English :: IO ())
```

## Validation and Linting

Always run these commands before committing changes:

### EditorConfig Compliance (MANDATORY)

**ALWAYS run this first before any commit**:

```bash
./scripts/check-editorconfig.sh
```

This script enforces:

- No trailing whitespace
- Final newlines on all files (except test/unit/\*\* files)

**If violations found**, fix them immediately with:

```bash
# Remove trailing whitespace from specific file:
sed -i 's/[[:space:]]*$//' filename

# Add final newline to specific file:
echo >> filename
```

### Linting

**Running HLint**:

- Manual linting: `hlint src/ test/ app/`
- HLint configuration in `.hlint.yaml`: uses `--cpp-simple` flag and ignores "Redundant pure" warnings

### Spell Checking

The repository includes comprehensive spell checking via GitHub Actions:

- Uses `check-spelling/check-spelling` with multiple dictionaries
- Includes CSS, LaTeX, software terms, Haskell, German, and English dictionaries
- Checks both file content and filenames
- Runs automatically on push and pull requests
- Configuration in `.github/actions/spelling/` directory

### Naming Conventions

**CRITICAL**: All function and variable names (also local variable names) in Haskell code MUST be spell-checkable and avoid abbreviations.

**Rules for naming**:

- **NO abbreviations**: Never use abbreviated names like `len`, `subseq`, `cfg`, `inst`, `idx`, `tmp`, `cnt`, `num`, `str`, `val`, etc.
- **Use full, descriptive names**: Instead of `len`, use `length` or `theLength`; instead of `subseq`, use `subsequence`
- **CamelCase for clarity**: Use camelCase to combine words clearly (e.g., `theLength`, `currentIndex`, `temporaryValue`)
- **Prefer clarity over brevity**: `numberOfElements` is better than `numElems` or `nElems`
- **Spell check friendly**: All names should pass spell checking with standard dictionaries or be obvious compound words

**Examples of good naming**:

```haskell
-- Good: Full, descriptive names
checkDifferentNamesInstance :: DifferentNamesInstance -> Maybe String
differentNamesEvaluation :: OutputCapable m => DifferentNamesInstance -> [(Name, Name)] -> Rated m
defaultDifferentNamesConfig :: DifferentNamesConfig
randomiseLayout :: RandomiseLayout a => a -> Int -> IO a

-- Good: Clear compound words with CamelCase
theLength :: Int
currentIndex :: Int
temporaryValue :: String
numberOfElements :: Int
```

**Examples of bad naming (DO NOT USE)**:

```haskell
-- Bad: Abbreviated names
len :: Int              -- Use: length, theLength, listLength
subseq :: [a] -> [a]    -- Use: subsequence
cfg :: Config           -- Use: config, configuration
inst :: Instance        -- Use: instance, theInstance
idx :: Int              -- Use: index, currentIndex
tmp :: String           -- Use: temporary, temporaryValue
cnt :: Int              -- Use: count, counter
num :: Int              -- Use: number, numberOfItems
```

**Exceptions**:

- Standard Haskell library functions and types (e.g., `putStrLn`, `Seq`)
- Standard Haskell conventions (e.g., `xs`, `x`, single-letter type variables)
- Loop variables in very short, localized contexts (e.g., `i`, `j`, `k` in list comprehensions)
- Widely accepted mathematical notation in domain-specific contexts (e.g., `n` for count in mathematical functions)
- Standard abbreviations from the problem domain (e.g., `cd` for "class diagram", `od` for "object diagram" when these are established terms in the codebase)

### Code Formatting

**MANDATORY .editorconfig compliance**:

```bash
# ALWAYS run before committing:
./scripts/check-editorconfig.sh
```

Follow `.editorconfig` standards (enforced by CI):

- **2-space indentation** (where specified)
- **LF line endings** (except test/unit/\*\* files)
- **TRIM TRAILING WHITESPACE** (except test/unit/\*\* files)
- **INSERT FINAL NEWLINE** (except test/unit/\*\* files)
- **175 character line limit** (160 for .als files)
- **No line length limits** for YAML, Markdown, or TeX files
- **Special handling** for test/unit/ files (formatting rules relaxed)

**Quick fix commands for violations**:

```bash
# Remove trailing whitespace:
sed -i 's/[[:space:]]*$//' filename

# Add final newline:
echo >> filename
```

## Haskell Development Guidelines

When writing Haskell code for this project, follow these best practices:

### Code Reuse and Refactoring

**Always look for refactoring opportunities**: Whenever adding functions, check whether there are opportunities to increase code reuse:

- Between the new function and pre-existing ones
- Between multiple functions being added in the same pull request
- Look for common patterns that can be extracted into helper functions
- Consider using higher-order functions to abstract common behaviors

### Unsafe Functions and Partiality

**Avoid `fromJust` in most cases** (in probably more than 90% of occurrences, use of `fromJust` is a mistake):

- `fromJust` is partial and can crash at runtime
- Use pattern matching on `Maybe` instead: `case maybeValue of Just x -> ...; Nothing -> ...`
- Use `fromMaybe` with a default value: `fromMaybe defaultValue maybeValue`
- Use monadic binding with `maybe` or `>>=` to handle `Nothing` cases gracefully
- Only use `fromJust` when you have a very strong proof that the value is always `Just`, and document why

### List Operations

**Prefer `nubOrd` or `nubSort` over `nub`**:

- Using `nub` is almost never a good idea due to O(n²) complexity
- Use `nubOrd` from `Data.List.NonEmpty` or similar for O(n log n) performance
- Use `nubSort` when you also want the result sorted
- These require an `Ord` constraint but are much more efficient

**Avoid writing explicit recursions on lists**:

- Writing your own recursions on lists is very rarely necessary
- Usually, list comprehensions or existing higher-order functions are a better fit
- Use functions like `map`, `filter`, `fold`, `zipWith`, `concatMap`, etc.
- Use list comprehensions for clear, declarative list transformations
- Combine higher-order functions to express complex transformations
- Only write explicit recursion when the logic truly doesn't fit existing abstractions

### Helper Libraries

**The `extra` package is a good source of helper functions**:

- Check the `extra` package before writing utility functions
- It provides many useful functions like `groupSort`, `nubOrd`, `whenJust`, etc.
- Familiarize yourself with its contents to avoid reinventing the wheel

### Numeric Types

**Make conscious decisions about using `Int` vs. using `Integer`**:

- Use `Int` for bounded integers with better performance (typically 64-bit)
- Use `Integer` for unbounded arbitrary-precision integers
- Consider whether overflow is a concern for your use case
- Document the choice if it's not obvious

**Use `fromIntegral` instead of `fromInteger` or `toInteger`**:

- `fromIntegral` is more general and works between any `Integral` types
- It combines `toInteger` and `fromInteger` in one step
- More flexible for refactoring when types change

### Type Definitions

**Mostly avoid introducing type synonyms via `type`**:

- `type` synonyms are just aliases and provide no type safety
- Use `newtype` when you want a distinct type with zero runtime overhead
- Use `data` when you need multiple constructors or more complex structures
- Both `newtype` and `data` provide better type safety and clearer error messages
- Reserve `type` only for very simple abbreviations where type safety doesn't matter

### Function Definitions

**Use anonymous functions where appropriate**:

- Sometimes not introducing a name is a good way of not introducing a bad name
- Use lambda functions (`\x -> ...`) for simple, inline transformations
- Prefer named functions when they have clear, meaningful names
- Avoid deeply nested anonymous functions that hurt readability

**Consider inlining single-use definitions**:

- If a named entity (like a `let`-introduced value or top-level function) has:
  - A very short definition, AND
  - Is used only exactly once in the rest of the code
- Then it is sometimes better to simply inline it directly instead
- Again: Sometimes not introducing a name is a good way of not introducing a bad name
- Balance this with readability - don't inline if it makes code harder to understand

### Module Organization

**Use explicit export lists**:

- Always specify what a module exports with an explicit export list
- Make export lists as short as possible in each module
- Only export what needs to be public
- This makes APIs clearer and prevents accidental exposure of internals
- Example: `module MyModule (foo, bar, MyType(..)) where`

**Mostly use explicit import lists**:

- Prefer `import ModuleName (foo, bar)` over `import ModuleName`
- Makes dependencies clear and prevents name conflicts
- Exception: Widely-used, standard modules like `Control.Monad` may use qualified imports
- Use qualified imports for modules with common names: `import qualified Data.Map as M`
- Explicit imports make it easier to find where functions come from

## Repository Structure

### Key Directories

- `src/Modelling/` -- Main library source code
  - `ActivityDiagram/` -- UML Activity Diagram tasks
  - `CdOd/` -- Class Diagram and Object Diagram tasks
  - `PetriNet/` -- Petri Net tasks
  - `Auxiliary/` -- Common utilities
- `app/` -- Command-line applications
- `example/` -- Example applications and configurations
- `legacy-app/` -- Legacy applications (includes Lexer/Parser)
- `test/` -- HSpec test suites
- `alloy/` -- Alloy specification files (.als)
  - `alloy/ad/` -- Activity Diagram specifications
  - `alloy/cd/` -- Class Diagram specifications
  - `alloy/petri/` -- Petri Net specifications

### Build Configuration Files

- `package.yaml` -- Hpack package configuration (generates .cabal file)
- `modelling-tasks.cabal` -- Generated Cabal file (DO NOT EDIT)
- `stack.yaml`, `stack-apps.yaml`, `stack-examples.yaml` -- Stack configurations
- `hie.yaml` -- Haskell IDE Engine configuration
- `.editorconfig` -- Code formatting standards for editors
- `.ghci` -- Default GHCi configuration with preloaded modules and imports

## Validation and Testing

### End-to-End Validation

After making changes, always validate:

1. **EditorConfig compliance**: `./scripts/check-editorconfig.sh` **MUST PASS**
2. **HLint does not complain**: `hlint src/ test/ app/`
3. **Build succeeds**: `stack --stack-yaml=stack-apps.yaml build`
4. **Tests pass**: `stack --stack-yaml=stack-apps.yaml test` (30+ minutes)
5. **App execution**: Test at least one app with `stack exec <app-name>`
6. **GHCi interaction**: Load examples and generate task instances

### Manual Testing Workflow

1. **Start GHCi**: `stack ghci --stack-yaml=stack-examples.yaml`
2. **Generate task instance**: Follow patterns in README.md for specific tasks
3. **Export to files**: Tasks generate LaTeX and Graphviz output in specified directories
4. **Verify outputs**: Check that .tex, .svg, .pdf files are created correctly
5. **Test validation**: Try sample answers with task validation functions

### Testing Specific Tasks

Different tasks can be tested by following the naming pattern in GHCi:

- Replace `NameCdError` with other task names (e.g., `MatchCdOd`, `SelectAS`)
- Change `English` to `German` for German language versions
- Tasks may require directory arguments (e.g., `"/tmp/"`) - check function signatures
- Import modules based on task type: `Modelling.CdOd.NameCdError`, `Modelling.ActivityDiagram.MatchAd`
