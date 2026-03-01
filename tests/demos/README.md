# lsp-mux demo fixtures

These fixtures back real-server integration tests in `tests/lsp-mux-tests.el`.

- `pyright-ruff/`: Python diagnostics fixture.
- `gopls-golangci/`: Go diagnostics fixture.
- `bash-shellcheck/`: Shell diagnostics fixture.
- `yaml-ansible/`: YAML/Ansible diagnostics fixture.
- `ts-eslint-tailwind/`: TypeScript + ESLint + Tailwind diagnostics fixture.

Tests auto-skip when required language servers are not installed locally.
