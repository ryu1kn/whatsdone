# What's Done API Client

**NOTE: After changing the app to use AWS Cognito for authentication, this tool is not working**

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

    **NOTE:** Login command doesn't work now as What's done's authentication mechanism has changed.

    ```sh
    $ ./apiClient login --config config.json
    ```

* Get all dones

    ```sh
    $ ./apiClient get-dones --config config.json
    ```
