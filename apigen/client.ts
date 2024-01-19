import queryString from "query-string";


export function getGet(body: GetRequest, fetchFn?: (input: RequestInfo, init?: RequestInit) => Promise<Response>): Promise<KVEntry> {
  let options: RequestInit = {
    credentials: "same-origin" as RequestCredentials,
    method: "GET",
    headers: {"Content-Type": "application/json;charset=utf-8"}
  };
  
  options.body = JSON.stringify(body);

  let params = {};
  return (fetchFn || window.fetch)(`/get` + "?" + queryString.stringify(params), options).then((response) => {
    return new Promise((resolve, reject) => {
      if (response.status !== 200) {
        return response.text().then((text) => reject({text, status: response.status}));
      } else {
        return response.json().then((json) => resolve(json));
      }
    });
  });
}

export function postSet(body: KVEntry, fetchFn?: (input: RequestInfo, init?: RequestInit) => Promise<Response>): Promise<NoContent> {
  let options: RequestInit = {
    credentials: "same-origin" as RequestCredentials,
    method: "POST",
    headers: {"Content-Type": "application/json;charset=utf-8"}
  };
  
  options.body = JSON.stringify(body);

  let params = {};
  return (fetchFn || window.fetch)(`/set` + "?" + queryString.stringify(params), options).then((response) => {
    return new Promise((resolve, reject) => {
      if (response.status !== 200) {
        return response.text().then((text) => reject({text, status: response.status}));
      } else {
        return response.json().then((json) => resolve(json));
      }
    });
  });
}