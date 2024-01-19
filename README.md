# Functional programming. Assignment # 4, HTTP API for KV Storage using Servant.

## Technologies used

- *Servant* web-framework
- *Aeson* JSON-parser
- *Xmlbf* XML-parser
- *optparse-applicative* CLI options
- *servant-typescript* + *aeson-typescript* TypeScript client generation
- *servant-docs* Markdown API docs generation

## Description

The API provides basic in-memory key-value storage via 2 endpoints

- `POST /set`- set a key-value mapping
- `GET /get` - get a value mapped to a key

JSON/XML request body is required for both APIs.
Details in the *Usage* block

## Usage

Use `stack run` to run a web-server (or any other preferable builder)

Supported types for both key and value are ints and strings.

### JSON

The type of a key/value is deduced automatically when using JSON.

`GET /get`

```json
{
    "key": "wow"
}
```

`POST /set`

```json
{
    "key": 14,
    "value": "hello"
}
```

`Content-Type` header should be set to `application/json;charset=utf-8`


### XML

The type of a key/value must be specified manually, since XML supports only string literals


```xml
<GetRequest>
    <Key>
        <KeyValue type="integer">213</KeyValue>
    </Key>
</GetRequest>
```

```xml
<SetRequest>
    <Key>
        <KeyValue type="integer">14</KeyValue>
    </Key>
    <Value>
        <KeyValue type="string">hello</KeyValue>
    </Value>
</SetRequest>
```

`Content-Type` header should be set to `application/xml;charset=utf-8`

### Client libraries generation

Use `stack run -- --generate-client` to generate TypeScript client libraries

The example of generated libraries can be found in `apigen` directory.


### Markdown docs generation

Use `stack run -- --generate-docs` to generate TypeScript client libraries

The example of generated docs can be found in `api-docs.md`.
