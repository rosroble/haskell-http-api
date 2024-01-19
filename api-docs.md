## GET /get

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`
    - `application/xml;charset=utf-8`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"key":{"contents":"key_to_lookup","tag":"KVString"}}
```

- Example (`application/xml;charset=utf-8`):

```xml
<GetRequest><Key>key_to_lookup</Key></GetRequest>
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"key":{"contents":"Alex","tag":"KVString"},"value":{"contents":22,"tag":"KVInteger"}}
```

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"key":{"contents":1024,"tag":"KVInteger"},"value":{"contents":"abc","tag":"KVString"}}
```

## POST /set

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`
    - `application/xml;charset=utf-8`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"key":{"contents":"Alex","tag":"KVString"},"value":{"contents":22,"tag":"KVInteger"}}
```

- Example (`application/xml;charset=utf-8`):

```xml
<KVEntry><Key>Alex</Key><Value>22</Value></KVEntry>
```

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"key":{"contents":1024,"tag":"KVInteger"},"value":{"contents":"abc","tag":"KVString"}}
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

