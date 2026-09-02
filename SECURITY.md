# Security Policy

## Supported versions

Security reports are primarily evaluated against the latest published MiraProt release.

The `main` branch may contain unreleased changes and is not guaranteed to represent a stable release.

## Reporting a vulnerability

Please **do not open a public GitHub Issue for a suspected security vulnerability**.

Use GitHub's private vulnerability reporting feature instead:

1. Open the MiraProt repository on GitHub.
2. Go to **Security**.
3. Open **Advisories**.
4. Select **Report a vulnerability**.
5. Provide enough information to reproduce and assess the issue.

This reporting method does not require a public project email address.

## What may qualify as a security issue

Examples include:

- unintended access to local files or directories
- path traversal or unsafe file handling
- unintended disclosure of user data to external services
- unexpected command or code execution
- vulnerabilities in the portable launcher or release-checking mechanism
- dependency or update behavior that could compromise the integrity of a MiraProt installation
- other behavior that could affect confidentiality, integrity, or availability

## What is usually not a security issue

Please use a normal GitHub Issue for:

- incorrect scientific or statistical results
- usability problems
- installation problems without a security impact
- crashes that do not expose sensitive information or create another security impact
- feature requests
- documentation errors

## Sensitive information

Do not include patient-identifying, personally identifying, confidential, unpublished, proprietary, or otherwise sensitive research data in reports unless it is strictly necessary and an appropriate private disclosure method has been agreed upon.

Synthetic examples are preferred whenever possible.

## Response expectations

MiraProt is maintained as an open-source research software project. Security reports are handled on a best-effort basis and no fixed response or remediation time is guaranteed.
