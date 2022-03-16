# Hassman

**H**askell p**ass**word **man**ager

_Password manager that kicks ass!_

## Usage

### Read password for domain

Copy password for a given `domain` and `username` to clipboard.

```sh
hassman -d domain.com -u username
```

### Create password for domain

Create a password for a `domain` and `username`

```sh
hassman -d domain.com -u username --create
```

or

```sh
hassman -d domain.com -u username -c
```

### List all passwords

```sh
hassman --list
```
