# What's done frontend app

## Local development

```sh
(cd app ; yarn run start:api)

# Open another terminal
(cd app ; yarn run start:dev)
```

Then open `http://localhost:8080`

## Tests

Run `yarn --cwd app test` for a single Vitest run, or
`yarn --cwd app test:watch` while developing. Browser tests select jsdom with
`@vitest-environment jsdom`. Tests run in the Australia/Melbourne timezone.
