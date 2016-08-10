//Provides: caml_obj_tag const (const)
//Requires: MlString
function caml_obj_tag (x) {
  if (x instanceof Array) return x[0];
  if (x instanceof MlString) return 252;
  if (typeof x === "function") return 247;
  return 1000;
}
