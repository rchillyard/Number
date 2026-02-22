# Publishing the Number Library

## Prerequisites

```bash
gpg --sign LICENSE
rm LICENSE.gpg
```

NOTE that this must be done in a terminal because gpg doesn't interact
well with other windows.

### Step 1: publish signed in SBT shell

- `publishSigned`
- `sonaUpload`

### Step 2: Publish to Maven Central (Browser)
- go to https://central.sonatype.com/publishing 
- Log in
- Click `Publish Component` (or `Drop`)
