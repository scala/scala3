import { readFile } from "node:fs/promises";

const linkerOutput = process.env["SCALAJS_LINKER_OUTPUT"];
console.log(`running tests from ${linkerOutput}`);
const helperModulesDir = process.env["HELPER_MODULES_DIR"];
console.log(`loading helpers from ${helperModulesDir}`);

async function loadModuleExports(moduleFile, importsObj) {
  const buffer = await readFile(moduleFile);
  const module = await WebAssembly.instantiate(buffer, importsObj);
  return module.instance.exports;
}

const i8ArrayAccess = await loadModuleExports(`${helperModulesDir}/i8ArrayAccess.wasm`);
const i16ArrayAccess = await loadModuleExports(`${helperModulesDir}/i16ArrayAccess.wasm`);
const i32ArrayAccess = await loadModuleExports(`${helperModulesDir}/i32ArrayAccess.wasm`);
const i64ArrayAccess = await loadModuleExports(`${helperModulesDir}/i64ArrayAccess.wasm`);
const f32ArrayAccess = await loadModuleExports(`${helperModulesDir}/f32ArrayAccess.wasm`);
const f64ArrayAccess = await loadModuleExports(`${helperModulesDir}/f64ArrayAccess.wasm`);

function makeWasmArray(arrayAccessModule, elems) {
  const result = arrayAccessModule.create(elems.length);
  for (let i = 0; i < elems.length; i++)
    arrayAccessModule.set(result, i, elems[i]);
  return result;
}

function wasmArrayFoldThenSetElem0(arrayAccessModule, elems, start, op, newElem0) {
  let acc = start;
  const len = arrayAccessModule.length(elems);
  for (let i = 0; i !== len; i++)
    acc = op(acc, arrayAccessModule.get(elems, i));
  arrayAccessModule.set(elems, 0, newElem0);
  return acc;
}

let foo_marker = 0;

const globalI8Array = makeWasmArray(i8ArrayAccess, [5, 4, 3, 2, 1]);
const globalI16Array = makeWasmArray(i16ArrayAccess, [5, 4, 3, 2, 1]);
const globalI32Array = makeWasmArray(i32ArrayAccess, [5, 4, 3, 2, 1]);
const globalI64Array = makeWasmArray(i64ArrayAccess, [5n, 4n, 3n, 2n, 1n]);
const globalF32Array = makeWasmArray(f32ArrayAccess, [5, 4, 3, 2, 1]);
const globalF64Array = makeWasmArray(f64ArrayAccess, [5, 4, 3, 2, 1]);

const importsObj = {
  "foo": {
    i32Times11: (x) => Math.imul(x, 11),
    i64Times11: (x) => BigInt.asIntN(64, x * 11n),
    f32Times11dot5: (x) => Math.fround(x * 11.5),
    f64Times11dot5: (x) => x * 11.5,

    getFooMarker: () => foo_marker,
    setFooMarker: (x) => { foo_marker = x; },

    throwRangeError: (x) => { throw new RangeError("" + x); },
  },

  "bar": {
    i8ArraySum: (xs) => wasmArrayFoldThenSetElem0(i8ArrayAccess, xs, 0, (a, b) => (a + b) << 24 >> 24, 66),
    i16ArraySum: (xs) => wasmArrayFoldThenSetElem0(i16ArrayAccess, xs, 0, (a, b) => (a + b) << 16 >> 16, 66),
    i32ArraySum: (xs) => wasmArrayFoldThenSetElem0(i32ArrayAccess, xs, 0, (a, b) => (a + b) | 0, 66),
    i64ArraySum: (xs) => wasmArrayFoldThenSetElem0(i64ArrayAccess, xs, 0n, (a, b) => BigInt.asIntN(64, a + b), 66n),
    f32ArraySum: (xs) => wasmArrayFoldThenSetElem0(f32ArrayAccess, xs, 0, (a, b) => Math.fround(a + b), 66),
    f64ArraySum: (xs) => wasmArrayFoldThenSetElem0(f64ArrayAccess, xs, 0, (a, b) => a + b, 66),

    getGlobalI8Array: () => globalI8Array,
    getGlobalI16Array: () => globalI16Array,
    getGlobalI32Array: () => globalI32Array,
    getGlobalI64Array: () => globalI64Array,
    getGlobalF32Array: () => globalF32Array,
    getGlobalF64Array: () => globalF64Array,
  },
};

const exports = await loadModuleExports(`${linkerOutput}/main.wasm`, importsObj);

let testFailures = [];

function assertEquals(expected, actual) {
  if (actual !== expected)
    throw new Error(`expected ${expected}; but got ${actual}`);
}

function assertOK(actual) {
  assertEquals(777, actual);
}

function assertThrows(exceptionClass, op) {
  try {
    op();
  } catch (e) {
    if (!(e instanceof exceptionClass))
      throw new Error(`expected a ${exceptionClass.name} to be thrown; but got ${e}`);
    return e;
  }
  throw new Error("expected an exception to be thrown; but nothing was thrown");
}

function test(testName, testBody) {
  console.log("----------------------");
  console.log(testName);
  try {
    testBody();
    console.log("OK");
  } catch (e) {
    testFailures.push(testName);
    console.error(e);
  }
}

test("numeric types exports", () => {
  assertEquals(20, exports.i32Times5(4));
  assertEquals(20n, exports.i64Times5(4n));
  assertEquals(26.125, exports.f32Times5dot5(4.75));
  assertEquals(26.125, exports.f64Times5dot5(4.75));

  assertEquals(535145149, exports.i32Times5(966022489));
  assertEquals(-1835996542583005695n, exports.i64Times5(-367199308516601139n));
  assertEquals(Math.fround(4.2659903), exports.f32Times5dot5(Math.fround(0.7756346)));
  assertEquals(3.0882886347573164, exports.f64Times5dot5(0.5615070245013303));

  assertEquals(187.75, exports.combineNumericTypes(11, 12n, 13.5, 14.75));
});

test("unit result exports", () => {
  assertEquals(undefined, exports.unitResult(456));
  assertEquals(456, exports.unitResultMarker());
});

test("elementary throw", () => {
  /* Note: we cannot check the payload, since we don't have the exception Tag.
   * The tag used by the Wasm module is not shared with us.
   */
  assertThrows(WebAssembly.Exception, () => exports.throwException(5));
});

test("numeric types imports", () => {
  assertOK(exports.numericImports());
});

test("unit result imports", () => {
  assertOK(exports.unitResultImports());
});

test("import functions that throw", () => {
  const e = assertThrows(RangeError, () => exports.importsThatThrow());
  assertEquals("666", e.message);
  assertEquals(3 * 7, exports.unitResultMarker());
});

test("i8 array exports", () => {
  const array = makeWasmArray(i8ArrayAccess, [5, 10, -17, 101, -120, -100, -95]);
  assertEquals(40, exports.i8ArraySum(array));
  assertEquals(5, i8ArrayAccess.get(array, 0)); // only the copy was mutated
});

test("i16 array exports", () => {
  const array = makeWasmArray(i16ArrayAccess, [5, 10, -17, 101, -120, -100, -95]);
  assertEquals(-216, exports.i16ArraySum(array));
  assertEquals(5, i16ArrayAccess.get(array, 0)); // only the copy was mutated
});

test("i32 array exports", () => {
  const array = makeWasmArray(i32ArrayAccess, [5, 10, -17, 101, -120, -100, -95]);
  assertEquals(-216, exports.i32ArraySum(array));
  assertEquals(5, i32ArrayAccess.get(array, 0)); // only the copy was mutated
});

test("i64 array exports", () => {
  const array = makeWasmArray(i64ArrayAccess, [5n, 10n, -17n, 101n, -120n, -100n, -95n]);
  assertEquals(-216n, exports.i64ArraySum(array));
  assertEquals(5n, i64ArrayAccess.get(array, 0)); // only the copy was mutated
});

test("f32 array exports", () => {
  const array = makeWasmArray(f32ArrayAccess, [5.125, 10.25, -17.375, 101.5, -120.625, -100.750, -95.875]);
  assertEquals(-217.75, exports.f32ArraySum(array));
  assertEquals(5.125, f32ArrayAccess.get(array, 0)); // only the copy was mutated
});

test("f64 array exports", () => {
  const array = makeWasmArray(f64ArrayAccess, [5.125, 10.25, -17.375, 101.5, -120.625, -100.750, -95.875]);
  assertEquals(-217.75, exports.f64ArraySum(array));
  assertEquals(5.125, f64ArrayAccess.get(array, 0)); // only the copy was mutated
});

test("i8 array result export", () => {
  const array = exports.getByteArray();
  assertEquals(3, i8ArrayAccess.get(array, 2));
  i8ArrayAccess.set(array, 0, 66);
  assertEquals(1, i8ArrayAccess.get(exports.getByteArray(), 0));
})

test("i16 array result export", () => {
  const array = exports.getShortArray();
  assertEquals(3, i16ArrayAccess.get(array, 2));
  i16ArrayAccess.set(array, 0, 66);
  assertEquals(1, i16ArrayAccess.get(exports.getShortArray(), 0));
})

test("i32 array result export", () => {
  const array = exports.getIntArray();
  assertEquals(3, i32ArrayAccess.get(array, 2));
  i32ArrayAccess.set(array, 0, 66);
  assertEquals(1, i32ArrayAccess.get(exports.getIntArray(), 0));
})

test("i64 array result export", () => {
  const array = exports.getLongArray();
  assertEquals(3n, i64ArrayAccess.get(array, 2));
  i64ArrayAccess.set(array, 0, 66n);
  assertEquals(1n, i64ArrayAccess.get(exports.getLongArray(), 0));
})

test("f32 array result export", () => {
  const array = exports.getFloatArray();
  assertEquals(3, f32ArrayAccess.get(array, 2));
  f32ArrayAccess.set(array, 0, 66);
  assertEquals(1, f32ArrayAccess.get(exports.getFloatArray(), 0));
})

test("f64 array result export", () => {
  const array = exports.getDoubleArray();
  assertEquals(3, f64ArrayAccess.get(array, 2));
  f64ArrayAccess.set(array, 0, 66);
  assertEquals(1, f64ArrayAccess.get(exports.getDoubleArray(), 0));
})

test("array param imports", () => {
  assertOK(exports.arrayParamImports());
});

test("array result imports", () => {
  assertOK(exports.arrayResultImports());
});

if (testFailures.length > 0) {
  console.error("----------------------");
  console.error("there were test failures:");
  for (const testName of testFailures)
    console.error(testName);
  process.exit(1);
}
