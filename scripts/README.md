# Scripts

Utility scripts for maintaining and validating the LeetcodeHero repository.

## Available Scripts

### validate_db.py

Validates the `cultivation_db.yaml` database against the filesystem structure.

**What it checks:**
- ✓ All links in `cultivation_db.yaml` point to existing directories
- ✓ All problem directories have corresponding database entries
- ✓ Problem IDs match their directory names
- ✓ No duplicate problem IDs exist in the database

**Usage:**
```bash
python3 scripts/validate_db.py
```

**Features:**
- No external dependencies (pure Python 3)
- Color-coded output for easy reading
- Detailed error reporting
- Exit code 0 for success, 1 for failures

**When to use:**
- After adding new problems
- After renaming directories
- After manually editing `cultivation_db.yaml`
- Before committing changes
- When debugging broken links

**Example output:**
```
LeetCode Hero - Database Validator
Repository: /Users/panyiping/EaglePlayground/LeetcodeHero
ℹ Loading database from cultivation_db.yaml
ℹ Found 101 problems in database

======================================================================
Validating Database Links
======================================================================
✓ All 101 links are valid!

======================================================================
Validation Summary
======================================================================
✓ All validations passed! Database is consistent with filesystem.
```

## Future Scripts (Planned)

### new_problem.sh
Creates a new problem directory with template files and adds entry to database.

```bash
scripts/new_problem.sh 3730 "New Problem Title"
```

### sync_status.py
Updates problem status based on git commit history and practice logs.

### generate_report.py
Generates progress reports and statistics from the cultivation database.

## Contributing

When adding new scripts:
1. Add executable permission: `chmod +x scripts/your_script.sh`
2. Include usage documentation in this README
3. Follow the existing naming conventions
4. Add comments explaining complex logic
