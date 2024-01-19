## GET /get

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`
    - `application/xml;charset=utf-8`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"key":"key_to_lookup"}
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
{"key":"Alex","value":22}
```

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"key":1024,"value":"abc"}
```

## POST /set

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`
    - `application/xml;charset=utf-8`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"key":"Alex","value":22}
```

- Example (`application/xml;charset=utf-8`):

```xml
<KVEntry><Key>Alex</Key><Value>22</Value></KVEntry>
```

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"key":1024,"value":"abc"}
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript

```

