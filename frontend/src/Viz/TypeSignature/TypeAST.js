// FFI for TypeSigViz.TypeAST

export const mkOk = (ast) => ({ ok: true, ast });
export const mkErr = (error) => ({ ok: false, error });

export const mkObj = (tag) => (fields) => {
  const obj = { tag };
  for (const { key, value } of fields) {
    obj[key] = value;
  }
  return obj;
};

export const mkStr = (s) => s;
export const mkArr = (arr) => arr;
export const mkNull = null;
