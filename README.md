# Hassman

**H**askell p**ass**word **man**ager

_Password manager that kicks ass!_

## Usage

### Initialize

Create a master password by running:

```sh
hassman --init
```

Type in desired master password in the prompt. Successive calls to `--init` will create newer versions of master password, invalidating the old ones.

(TODO: rollback feature)

All other commands will prompt for master password.

### Create a password for domain

Create a password for a `domain` and `username`

```sh
hassman -d domain.com -u username --create
```

or

```sh
hassman -d domain.com -u username -c
```

### Copy password to clipboard

Copy password for a given `domain` and `username` to clipboard.

```sh
hassman -d domain.com -u username
```

### List all passwords

```sh
hassman --list
```
