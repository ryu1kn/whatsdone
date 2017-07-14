# What's Done API Client

## Build

```sh
$ make build
```

## Usage

```sh
$ ./apiClient --help
```

Many commands require config file. Sample should look like:

```json
{
  "apiEndpoint": "https://whatsdone-api.ryuichi.io",
  "email": "LOGIN EMAIL ADDRESS",
  "password": "LOGIN PASSWORD",
  "sessionFile": "file-name-to-store-session-id.txt"
}
```

## Command Examples

* Login

    ```sh
    $ ./apiClient login --config config.json
    ```

* Get all dones

    ```sh
    $ ./apiClient get-dones --config config.json
    ```
