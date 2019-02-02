# fingerd

### Setup:

Install dependencies and build:

```
stack build
```

Create database:

Use this from `stack ghci`:
```
createDatabase
```

### Usage:

```
sudo `stack exec which main`
```

This will run two servers:

1. `finger` server on port `79`
1. `userModify` server on port `12345`

* Query `fingerd` server using `finger` command:

```sh
finger tejas@localhost
```

* To add/modify users, you will need to connect to the `userModify` server using `telnet`:


```sh
telnet localhost 12345
```

Then use following commands:

```
Add example2,/bin/bash,/home,Example-2,191919191
Update example2,/bin/2222,/home/2222,Example-2222,1222212222
```

### CLI executable:

```sh
$(stack exec which userActionsExe) "Add" "example3,/bin/bash,/home/example1,Example 1,1919191919"

$(stack exec which userActionsExe) "Update" "example3,/bin/sh33,/home/examplesh,Example sh,2919191919"
```
