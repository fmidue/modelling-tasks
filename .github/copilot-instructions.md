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

### 🔴 NEVER COMMIT CODE THAT DOESN'T BUILD

**ABSOLUTE REQUIREMENT**: Every commit MUST successfully build with `stack --stack-yaml=stack-apps.yaml test --no-run-tests modelling-tasks`.

**BEFORE ANY COMMIT**: Run `stack --stack-yaml=stack-apps.yaml test --no-run-tests modelling-tasks` to validate the code compiles:

```bash
stack --stack-yaml=stack-apps.yaml test --no-run-tests modelling-tasks
```

Or for the full application suite:

```bash
stack --stack-yaml=stack-apps.yaml test --no-run-tests
```

**If build fails**: Fix all compilation errors before committing:

- Review the error messages carefully
- Fix all type errors, missing imports, and syntax issues
- Re-run `stack --stack-yaml=stack-apps.yaml test --no-run-tests modelling-tasks` until it succeeds
- Only then proceed with committing

**IF `stack --stack-yaml=stack-apps.yaml test --no-run-tests modelling-tasks` FAILS**:

- **DO NOT COMMIT**
- **DO NOT USE `report_progress`**
- **FIX ALL BUILD ERRORS FIRST**

**Build times**: Remember that builds can take 30-45 minutes. Set appropriate timeout values (60+ minutes) and never cancel builds.

### 🔴 NEVER COMMIT CODE WHERE TESTS DON'T COMPILE OR EXAMPLE DIRECTORY FAILS

**ABSOLUTE REQUIREMENT**: The test suite must always compile, and the example directory must always compile and run successfully.

**BEFORE ANY COMMIT**: Run these commands to validate:

```bash
# Test suite must compile (must succeed)
stack --stack-yaml=stack-apps.yaml test --no-run-tests

# Example directory must compile and run (must succeed)
stack --stack-yaml=stack-examples.yaml test
```

**If either fails**: Fix all errors before committing. Only then proceed with committing.

**IF VALIDATION FAILS**:

- **DO NOT COMMIT**
- **DO NOT USE `report_progress`**
- **FIX ALL ERRORS FIRST**

### ⏰ NEVER CANCEL BUILDS OR TESTS

- **Project builds**: 30-45 minutes (set timeout to 60+ minutes)
- **Targeted test runs**: 5-15 minutes (set timeout to 30+ minutes)
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

**CRITICAL**: **NEVER run the full test suite**. Always use targeted tests with `--match` patterns.

**Targeted testing examples**:
- `stack test --test-arguments="-m SelectAS"` -- Test specific module
- `stack test --test-arguments="-m Modelling.CdOd"` -- Test category
- `stack test --test-arguments="-m \"is valid\""` -- Test specific description

**Additional test options**:
  - `--test-arguments="--skip-needs-tuning"` -- excludes unstable/long-running tests
  - `--test-arguments="--maximum-generated-tests=50"` -- limits test case generation

**Combine matching with options**:
- `stack test --test-arguments="-m SelectAS --skip-needs-tuning --maximum-generated-tests=10"`

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

### Build Success (MANDATORY)

**CRITICAL**: Code must successfully build before any commit:

```bash
stack --stack-yaml=stack-apps.yaml test --no-run-tests modelling-tasks
```

For the full application suite:

```bash
stack --stack-yaml=stack-apps.yaml test --no-run-tests
```

**Never commit code that doesn't build**. This is a fundamental requirement.

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

### Git Diff Management

**CRITICAL**: Always favor minimal Git diffs over code alignment when modifying existing code.

This principle is particularly important when working with Haskell records:

- **When modifying record type definitions**: Only change the lines that need to be changed
- **When modifying record value assignments**: Only change the lines that need to be changed
- **NEVER realign existing fields** just to make them line up with new or modified fields
- **When adding new fields to records (or to other entity listings in the code)**:
  - **PREFERRED**: Add new fields somewhere BEFORE the last field (not at the end) to minimize the diff
  - Adding at the end requires modifying the previously-last field to add a trailing comma (1 existing line changed + new lines added)
  - Adding before the last field requires no modifications to existing lines (0 existing lines changed + new lines added)
  - This same principle **also** applies to export lists, import lists, and other comma-separated lists given as separate lines in the code

**Examples for record type definitions**:

❌ **BAD** - Realigning all fields (creates large diff):

```haskell
-- Adding maxDisplayedSolutions field
-- DO NOT realign other fields like this:
data DeadlockInstance s t = DeadlockInstance {
  drawUsing             :: GraphvizCommand,  -- realigned (unnecessary change)
  minLength             :: Int,              -- realigned (unnecessary change)
  noLongerThan          :: Maybe Int,        -- realigned (unnecessary change)
  petriNet              :: Net s t,          -- realigned (unnecessary change)
  showPlaceNames        :: Bool,             -- realigned (unnecessary change)
  withLengthHint        :: Maybe Int,        -- realigned (unnecessary change)
  withMinLengthHint     :: Bool,             -- realigned (unnecessary change)
  solutions             :: Either [t] [[t]], -- realigned and added trailing comma (unnecessary changes)
  maxDisplayedSolutions :: Maybe Int         -- new field
```

✅ **GOOD** - Minimal diff (only changed lines):

```haskell
-- Adding maxDisplayedSolutions field
-- Keep existing alignment, only modify necessary lines:
data DeadlockInstance s t = DeadlockInstance {
  drawUsing         :: GraphvizCommand,
  minLength         :: Int,
  noLongerThan      :: Maybe Int,
  petriNet          :: Net s t,
  showPlaceNames    :: Bool,
  withLengthHint    :: Maybe Int,
  withMinLengthHint :: Bool,
  maxDisplayedSolutions :: Maybe Int,  -- new field (added without realigning others and without requiring trailing comma in the last line)
  solutions         :: Either [t] [[t]]
```

**Examples for record value assignments**:

❌ **BAD** - Realigning all fields and changing order (creates large diff):

```haskell
-- Replacing showSolution field with two new fields: instanceMaxDisplayedSolutions and solutions
-- DO NOT realign other fields or reorder like this:
defaultDeadlockInstance = DeadlockInstance {
  drawUsing                     = Circo,     -- realigned (unnecessary change)
  minLength                     = 6,         -- realigned (unnecessary change)
  noLongerThan                  = Nothing,   -- realigned (unnecessary change)
  petriNet                      = fst example, -- realigned (unnecessary change)
  showPlaceNames                = False,     -- realigned (unnecessary change)
  -- THIS IS WHERE the showSolution field was previously
  withLengthHint                = Just 9,    -- realigned (unnecessary change)
  withMinLengthHint             = True,      -- realigned and added trailing comma (unnecessary changes)
  instanceMaxDisplayedSolutions = Nothing,   -- new field (replacing showSolution)
  solutions                     = Left []    -- new field (replacing showSolution)
  }
```

✅ **GOOD** - Minimal diff (only changed lines):

```haskell
-- Replacing showSolution field with two new fields: instanceMaxDisplayedSolutions and solutions
-- Keep existing alignment, replace field at same location, no reordering:
defaultDeadlockInstance = DeadlockInstance {
  drawUsing         = Circo,
  minLength         = 6,
  noLongerThan      = Nothing,
  petriNet          = fst example,
  showPlaceNames    = False,
  instanceMaxDisplayedSolutions = Nothing,  -- new field (replacing showSolution)
  solutions         = Left [],              -- new field (replacing showSolution)
  withLengthHint    = Just 9,
  withMinLengthHint = True
  }
```

**Note**: The `solutions` field uses padding spaces here to fit the previous alignment. This is fine because:

- It doesn't change the line count of the diff
- It maintains consistency with existing field alignment
- What would be problematic is realigning all existing fields to match the new longer `instanceMaxDisplayedSolutions` field

**Rationale**:

- Smaller diffs are easier to review
- Smaller diffs reduce merge conflicts
- Smaller diffs make Git history more meaningful
- Code alignment is less important than diff clarity
- The goal is to show what actually changed, not to make everything perfectly aligned

### Code Reuse and Refactoring

**Always look for refactoring opportunities**: Whenever adding functions, check whether there are opportunities to increase code reuse:

- Between the new function and pre-existing ones
- Between multiple functions being added in the same pull request

### Unsafe Functions and Partiality

**Avoid `fromJust` in most cases**:

- Use pattern matching on `Maybe` instead: `case maybeValue of Just x -> ...; Nothing -> ...`
- Use `fromMaybe` with a default value: `fromMaybe defaultValue maybeValue`
- Use `maybe` to handle `Nothing` cases gracefully
- Only use `fromJust` when you have a very strong proof that the value is always `Just`, and document why

### List Operations

**Prefer `nubOrd` or `nubSort` over `nub`**:

- Use `nubOrd` for better performance
- Use `nubSort` when you also want the result sorted
- These require an `Ord` constraint but are much more efficient

**Avoid writing explicit recursions on lists**:

- Only write explicit recursion when the logic truly doesn't fit existing abstractions
- Usually, list comprehensions or existing higher-order functions are a better fit

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
- Balance this with readability - don't inline if it makes code harder to understand

### Deriving ToDoc and Reader Instances

**CRITICAL**: All Config and Instance data types MUST derive `ToDoc` and `Reader` instances for Autotool compatibility.

**Background**:

- This project generates tasks that are used in [Autotool](https://git.uni-due.de/fmi/autotool-dev)
- Autotool requires `ToDoc` and `Reader` instances for serialization/deserialization
- Forgetting these instances causes build failures in Autotool (not locally)

**When to derive ToDoc and Reader**:

1. **Always derive for Config types**: Any data type named `*Config` (e.g., `MatchAdConfig`, `NameCdErrorConfig`)
2. **Always derive for Instance types**: Any data type named `*Instance` (e.g., `MatchAdInstance`, `SelectASInstance`)
3. **Always derive for nested types**: Any custom data type used as a field in Config or Instance types
4. **Always derive for task-related enums**: Enumeration types used in task configuration or instances

**Required language extensions**:

```haskell
{-# LANGUAGE DeriveAnyClass #-}  -- Required for deriving Reader and ToDoc
{-# LANGUAGE DeriveGeneric #-}   -- Required for Generic derivation
```

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

1. **Build succeeds**: `stack --stack-yaml=stack-apps.yaml test --no-run-tests modelling-tasks` or `stack --stack-yaml=stack-apps.yaml test --no-run-tests` **MUST PASS BEFORE COMMIT**
2. **Test suite compiles**: `stack --stack-yaml=stack-apps.yaml test --no-run-tests` **MUST PASS BEFORE COMMIT**
3. **Example directory compiles and tests pass**: `stack --stack-yaml=stack-examples.yaml test` **MUST PASS BEFORE COMMIT**
4. **EditorConfig compliance**: `./scripts/check-editorconfig.sh` **MUST PASS**
5. **HLint does not complain**: `hlint src/ test/ app/` **MUST PASS WITHOUT EVEN JUST SUGGESTIONS**
6. **Targeted tests pass**: Run targeted tests for code you modified using `--match` patterns
7. **App execution** (if applicable): Test relevant apps with `stack exec <app-name>`
8. **GHCi interaction** (if applicable): Load examples and generate task instances

**NEVER run the full test suite** (`stack test` or `stack --stack-yaml=stack-apps.yaml test` without `--match`). Always use targeted testing.

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

### Test Matching with HSpec

This repository uses **hspec-discover** for automatic test discovery. Tests are organized hierarchically, and matching follows a specific pattern.

**Test Structure Hierarchy**:

Tests are matched using a hierarchical path consisting of:

1. **Module name** (from the file path, e.g., `Modelling.ActivityDiagram.SelectAS`)
2. **describe blocks** (top-level grouping in specs)
3. **context blocks** (optional nested grouping)
4. **it blocks** (individual test names)

**How to Use --match**:

The `--match` (or `-m`) option accepts patterns that match against the full hierarchical test path. Matching is substring-based and case-sensitive.

**CRITICAL QUOTING RULES**: When using `stack test --test-arguments`, the entire argument string is already in double quotes. Therefore:

- **DO NOT** use single quotes around patterns - they become part of the pattern itself
- For patterns with spaces, use escaped double quotes: `\"`
- For simple patterns without spaces, no quotes are needed

**IMPORTANT**: When using `stack test --test-arguments`, patterns with spaces MUST be quoted with escaped quotes:

```bash
# CORRECT - Pattern with spaces requires escaped quotes
stack test --test-arguments="-m \"is valid\""

# CORRECT - Simple patterns without spaces don't need quotes
stack test --test-arguments="-m SelectAS"

# WRONG - Do NOT use single quotes around the pattern
stack test --test-arguments="--match 'SelectAS'"  # This will match 0 tests!

# WRONG - This is also incorrect (single quotes become part of the pattern)
stack test --test-arguments="-m 'Modelling.CdOd'"  # Will match nothing!
```

**Examples with actual tests from this repository**:

```bash
# Match all tests in the SelectAS module (substring match works!)
stack test --test-arguments="-m SelectAS"

# Match all tests in the MatchCdOd module using full path
stack test --test-arguments="-m Modelling.CdOd.MatchCdOd"

# Match all tests in the ActivityDiagram category
stack test --test-arguments="-m Modelling.ActivityDiagram"

# Match all tests in the CdOd category
stack test --test-arguments="-m Modelling.CdOd"

# Match a specific test description across all modules (needs escaped quotes)
stack test --test-arguments="-m \"is valid\""

# Combine with other test options
stack test --test-arguments="-m SelectAS --skip-needs-tuning --maximum-generated-tests=10"
```

**Common Test Modules**:

Based on the test directory structure, here are the main test modules:

- `Modelling.ActivityDiagram.SelectAS` - SelectAS task tests
- `Modelling.ActivityDiagram.MatchAd` - MatchAd task tests
- `Modelling.ActivityDiagram.EnterAS` - EnterAS task tests
- `Modelling.ActivityDiagram.MatchPetri` - MatchPetri task tests
- `Modelling.ActivityDiagram.SelectPetri` - SelectPetri task tests
- `Modelling.CdOd.MatchCdOd` - MatchCdOd task tests
- `Modelling.CdOd.NameCdError` - NameCdError task tests
- `Modelling.CdOd.DifferentNames` - DifferentNames task tests
- `Modelling.CdOd.RepairCd` - RepairCd task tests
- `Modelling.CdOd.SelectValidCd` - SelectValidCd task tests
- `Modelling.PetriNet.Types` - Petri net type tests
- `Modelling.PetriNet.Reach.Reach` - Petri net reachability tests

**Finding Test Names**:

To see available test names and their hierarchy:

```bash
# List all test specs (shows full tree)
stack test --test-arguments="--dry-run"

# Filter and view specific category
stack test --test-arguments="-m Modelling.ActivityDiagram --dry-run"
```

**Best Practices**:

- **Simple substring matching works**: `SelectAS` will match `Modelling.ActivityDiagram.SelectAS`
- **Use `--dry-run` to verify**: Always test your pattern with `--dry-run` first to see what will run
- **Quote patterns with spaces**: Use `-m \"is valid\"` with escaped quotes for multi-word patterns
- **Be specific to avoid over-matching**: `SelectAS` is better than just `Select` which might match multiple modules
- **Substring matching is powerful**: `Modelling.CdOd` matches all class/object diagram tests
