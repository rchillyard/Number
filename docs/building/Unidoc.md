# GitHub Pages Deployment for Number Library Unidoc

## Prerequisites

- Personal access token with `repo` scope stored in your Git remote URL:
  ```bash
  git remote set-url origin https://rchillyard:YOUR_TOKEN@github.com/rchillyard/Number.git
  ```

## Full Deployment Steps

Run these commands from the root of your project, starting on your normal working branch (e.g. `main`):

### Step 1: Generate the unified docs

```bash
sbt unidoc
```

### Step 2: Copy docs to a safe temporary location

This is essential — switching branches will otherwise lose the generated docs.

```bash
cp -r target/scala-3.7.3/unidoc ~/unidoc-temp
```

### Step 3: Switch to the gh-pages branch and clean it

```bash
git checkout gh-pages
git rm -rf .
```

### Step 4: Copy the docs and create a redirect index

```bash
mkdir -p api
cp -r ~/unidoc-temp/* api/
echo '<meta http-equiv="refresh" content="0; url=api/index.html">' > index.html
```

### Step 5: Commit and push

```bash
git add .
git commit -m "Update unified API documentation"
git push -u origin gh-pages --force
```

### Step 6: Switch back to your working branch and clean up

```bash
git checkout main
rm -rf ~/unidoc-temp
```

## Notes

- The `--force` flag on push is safe here because `gh-pages` contains only generated
  documentation and nothing worth preserving from the remote.
- If `git rm -rf .` complains there is nothing to remove, just skip it.
- The docs will be live at: https://rchillyard.github.io/Number/
- GitHub Pages may take a minute or two to update after pushing.
- If the Scala version changes from `3.7.3`, update the path in Step 2 accordingly.
- If you need a new token for GitHub, go to https://github.com/settings/tokens.