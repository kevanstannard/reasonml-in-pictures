type nodeError = Js.nullable(Js.Exn.t);

type nodeValue('a) = Js.nullable('a);

type nodeCallback('a) = (. nodeError, nodeValue('a)) => unit;

[@bs.module "fs"]
external readFile: (string, string, nodeCallback(string)) => unit =
  "readFile";