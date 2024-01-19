interface IGetRequest {
  key: KeyValueType;
}

interface IKVEntry {
  key: KeyValueType;
  value: KeyValueType;
}

interface IKVInteger {
  tag: "KVInteger";
  contents: number;
}

interface IKVString {
  tag: "KVString";
  contents: string;
}

type GetRequest = IGetRequest;

type INoContent = void[];

type KVEntry = IKVEntry;

type KeyValueType = IKVString | IKVInteger;

type NoContent = INoContent;