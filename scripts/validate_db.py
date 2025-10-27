#!/usr/bin/env python3
"""
Validate cultivation_db.yaml against filesystem structure.

This script checks:
1. All links in cultivation_db.yaml point to existing directories
2. All problem directories have corresponding entries in the database
3. Problem IDs match directory names
4. No duplicate problem IDs in database

Usage:
    python3 scripts/validate_db.py
"""

import os
import re
import sys
from pathlib import Path


class Colors:
    """ANSI color codes for terminal output"""
    RED = '\033[91m'
    GREEN = '\033[92m'
    YELLOW = '\033[93m'
    BLUE = '\033[94m'
    MAGENTA = '\033[95m'
    CYAN = '\033[96m'
    RESET = '\033[0m'
    BOLD = '\033[1m'


def print_header(text):
    """Print a section header"""
    print(f"\n{Colors.BOLD}{Colors.CYAN}{'=' * 70}{Colors.RESET}")
    print(f"{Colors.BOLD}{Colors.CYAN}{text}{Colors.RESET}")
    print(f"{Colors.BOLD}{Colors.CYAN}{'=' * 70}{Colors.RESET}")


def print_success(text):
    """Print success message"""
    print(f"{Colors.GREEN}✓ {text}{Colors.RESET}")


def print_error(text):
    """Print error message"""
    print(f"{Colors.RED}✗ {text}{Colors.RESET}")


def print_warning(text):
    """Print warning message"""
    print(f"{Colors.YELLOW}⚠ {text}{Colors.RESET}")


def print_info(text):
    """Print info message"""
    print(f"{Colors.BLUE}ℹ {text}{Colors.RESET}")


def extract_problem_id_from_dir(dirname):
    """Extract problem ID from directory name like '001_two_sum'"""
    match = re.match(r'^(\d+)_', dirname)
    return int(match.group(1)) if match else None


def parse_cultivation_db(file_path):
    """Simple YAML parser for cultivation_db.yaml (no external dependencies)"""
    problems = []

    with open(file_path, 'r', encoding='utf-8') as f:
        content = f.read()

    # Split by problem entries (lines starting with "  - id:")
    entries = re.split(r'\n  - id: ', content)

    for entry in entries[1:]:  # Skip the header before first entry
        problem = {}

        # Extract id
        id_match = re.match(r'^(\d+)', entry)
        if id_match:
            problem['id'] = int(id_match.group(1))

        # Extract title
        title_match = re.search(r'title:\s*"([^"]+)"', entry)
        if title_match:
            problem['title'] = title_match.group(1)

        # Extract link
        link_match = re.search(r'link:\s*"?([^\n"]+)"?', entry)
        if link_match:
            problem['link'] = link_match.group(1)

        if problem.get('id'):
            problems.append(problem)

    return problems


def validate_links(problems, repo_root):
    """Validate that all links in database point to existing directories"""
    print_header("Validating Database Links")

    broken_links = []
    valid_links = 0

    for problem in problems:
        problem_id = problem.get('id')
        link = problem.get('link', '').strip().strip('"').strip("'")

        if not link:
            print_warning(f"Problem {problem_id}: No link specified")
            continue

        # Remove ./ prefix if present
        link = link.lstrip('./')
        full_path = repo_root / link

        if full_path.exists() and full_path.is_dir():
            valid_links += 1
        else:
            broken_links.append({
                'id': problem_id,
                'title': problem.get('title', 'Unknown'),
                'link': link,
                'full_path': full_path
            })

    if broken_links:
        print_error(f"Found {len(broken_links)} broken links:")
        for broken in broken_links:
            print(f"  Problem {broken['id']}: {broken['title']}")
            print(f"    Link: {broken['link']}")
            print(f"    Expected: {broken['full_path']}")
    else:
        print_success(f"All {valid_links} links are valid!")

    return len(broken_links) == 0


def validate_problem_coverage(problems, repo_root):
    """Check if all problem directories have database entries"""
    print_header("Validating Problem Coverage")

    problems_dir = repo_root / 'problems'
    if not problems_dir.exists():
        print_error("problems/ directory not found")
        return False

    # Get all problem IDs from database
    db_ids = {p.get('id') for p in problems if p.get('id')}

    # Get all problem directories
    problem_dirs = [d for d in os.listdir(problems_dir)
                   if os.path.isdir(problems_dir / d) and not d.startswith('.')]

    missing_from_db = []

    for dirname in sorted(problem_dirs):
        problem_id = extract_problem_id_from_dir(dirname)
        if problem_id and problem_id not in db_ids:
            missing_from_db.append({
                'id': problem_id,
                'dirname': dirname
            })

    if missing_from_db:
        print_warning(f"Found {len(missing_from_db)} directories not in database:")
        for missing in missing_from_db:
            print(f"  Problem {missing['id']}: {missing['dirname']}")
    else:
        print_success(f"All {len(problem_dirs)} problem directories are tracked in database!")

    # Also check for extra entries in database
    dir_ids = {extract_problem_id_from_dir(d) for d in problem_dirs}
    dir_ids = {i for i in dir_ids if i is not None}

    extra_in_db = db_ids - dir_ids
    if extra_in_db:
        print_warning(f"Found {len(extra_in_db)} database entries without directories:")
        for problem_id in sorted(extra_in_db):
            problem = next((p for p in problems if p.get('id') == problem_id), None)
            if problem:
                print(f"  Problem {problem_id}: {problem.get('title', 'Unknown')}")

    return len(missing_from_db) == 0 and len(extra_in_db) == 0


def validate_id_consistency(problems, repo_root):
    """Validate that problem IDs match directory names"""
    print_header("Validating ID Consistency")

    mismatches = []

    for problem in problems:
        problem_id = problem.get('id')
        link = problem.get('link', '').strip().strip('"').strip("'").lstrip('./')

        if not link or not problem_id:
            continue

        # Extract directory name from link
        dirname = Path(link).name
        dir_id = extract_problem_id_from_dir(dirname)

        if dir_id and dir_id != problem_id:
            mismatches.append({
                'db_id': problem_id,
                'dir_id': dir_id,
                'title': problem.get('title', 'Unknown'),
                'link': link
            })

    if mismatches:
        print_error(f"Found {len(mismatches)} ID mismatches:")
        for mismatch in mismatches:
            print(f"  Database ID: {mismatch['db_id']}, Directory ID: {mismatch['dir_id']}")
            print(f"    Title: {mismatch['title']}")
            print(f"    Link: {mismatch['link']}")
    else:
        print_success("All problem IDs match their directory names!")

    return len(mismatches) == 0


def validate_duplicate_ids(problems):
    """Check for duplicate problem IDs in database"""
    print_header("Validating Duplicate IDs")

    id_counts = {}
    for problem in problems:
        problem_id = problem.get('id')
        if problem_id:
            id_counts[problem_id] = id_counts.get(problem_id, 0) + 1

    duplicates = {pid: count for pid, count in id_counts.items() if count > 1}

    if duplicates:
        print_error(f"Found {len(duplicates)} duplicate problem IDs:")
        for problem_id, count in duplicates.items():
            print(f"  Problem {problem_id}: appears {count} times")
    else:
        print_success(f"All {len(id_counts)} problem IDs are unique!")

    return len(duplicates) == 0


def print_summary(all_valid):
    """Print final summary"""
    print_header("Validation Summary")

    if all_valid:
        print_success("All validations passed! Database is consistent with filesystem.")
        print_info(f"Total checks completed successfully")
    else:
        print_error("Some validations failed. Please review the issues above.")
        print_info("Fix the issues and run validation again")


def main():
    """Main validation function"""
    # Determine repository root
    script_dir = Path(__file__).parent
    repo_root = script_dir.parent

    print(f"{Colors.BOLD}LeetCode Hero - Database Validator{Colors.RESET}")
    print(f"Repository: {repo_root}")

    # Load cultivation_db.yaml
    db_path = repo_root / 'cultivation_db.yaml'

    if not db_path.exists():
        print_error(f"cultivation_db.yaml not found at {db_path}")
        sys.exit(1)

    print_info(f"Loading database from {db_path.name}")

    try:
        problems = parse_cultivation_db(db_path)
    except Exception as e:
        print_error(f"Failed to parse database: {e}")
        sys.exit(1)

    print_info(f"Found {len(problems)} problems in database")

    # Run all validations
    results = []
    results.append(validate_links(problems, repo_root))
    results.append(validate_problem_coverage(problems, repo_root))
    results.append(validate_id_consistency(problems, repo_root))
    results.append(validate_duplicate_ids(problems))

    all_valid = all(results)

    # Print summary
    print_summary(all_valid)

    # Exit with appropriate code
    sys.exit(0 if all_valid else 1)


if __name__ == '__main__':
    main()
