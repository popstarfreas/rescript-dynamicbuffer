module Array = Js.Array2
module Buffer = NodeJs.Buffer
type t = {
    past: array<Buffer.t>,
    mutable current: Buffer.t,
    mutable offset: int, // the current unwritten byte location
}
// offset = 1; [ 5, null, null ]
// This function will ensure there is enough space to write n bytes
let accountForSpace = (self: t, ~numberOfBytesToWrite: int) => {
    let len = self.current->Buffer.length
    let spaceLeft = len - self.offset
    if (spaceLeft - numberOfBytesToWrite) < 0 {
        // Slice the buffer in-case there are some bytes we are not writing to
        let data = self.current->Buffer.slice(~start=0, ~end_=self.offset)
        let newLen = len * 2 >= numberOfBytesToWrite ? len * 2 : numberOfBytesToWrite * 2
        self.past->Array.push(data)->ignore
        self.current = Buffer.allocUnsafe(newLen)
        self.offset = 0
    }
}

let ensureGreaterThanZero = n => if n <= 0 {
    1
} else {
    n
}

let make = (~initialSize=1024, ()): t => {
    past: [],
    current: Buffer.allocUnsafe(ensureGreaterThanZero(initialSize)),
    offset: 0,
}
let addInt8 = (self: t, int8: int): t => {
    self->accountForSpace(~numberOfBytesToWrite=1)
    self.current->Buffer.writeInt8(int8->land(0xFF), ~offset=self.offset)->ignore
    self.offset = self.offset + 1
    self
}
let addUint8 = (self: t, uint8: int): t => {
    self->accountForSpace(~numberOfBytesToWrite=1)
    self.current->Buffer.writeUint8(uint8->land(0xFF), ~offset=self.offset)->ignore
    self.offset = self.offset + 1
    self
}
let addIntLE = (self: t, n: int, ~numberOfBytesToWrite: int): t => {
    self->accountForSpace(~numberOfBytesToWrite)
    self.current->Buffer.writeIntLE(n, ~offset=self.offset, ~length=numberOfBytesToWrite)->ignore
    self.offset = self.offset + numberOfBytesToWrite
    self
}
let addIntBE = (self: t, n: int, ~numberOfBytesToWrite: int): t => {
    self->accountForSpace(~numberOfBytesToWrite)
    self.current->Buffer.writeIntBE(n, ~offset=self.offset, ~length=numberOfBytesToWrite)->ignore
    self.offset = self.offset + numberOfBytesToWrite
    self
}
let addUintBE = (self: t, n: int, ~numberOfBytesToWrite: int): t => {
    self->accountForSpace(~numberOfBytesToWrite)
    self.current->Buffer.writeUintBE(n, ~offset=self.offset, ~length=numberOfBytesToWrite)->ignore
    self.offset = self.offset + numberOfBytesToWrite
    self
}
let addUintLE = (self: t, n: int, ~numberOfBytesToWrite: int): t => {
    self->accountForSpace(~numberOfBytesToWrite)
    self.current->Buffer.writeUintLE(n, ~offset=self.offset, ~length=numberOfBytesToWrite)->ignore
    self.offset = self.offset + numberOfBytesToWrite
    self
}
let addFloatBE = (self: t, n: float): t => {
    self->accountForSpace(~numberOfBytesToWrite=4)
    self.current->Buffer.writeFloatBE(n, ~offset=self.offset)->ignore
    self.offset = self.offset + 4
    self
}
let addFloatLE = (self: t, n: float): t => {
    self->accountForSpace(~numberOfBytesToWrite=4)
    self.current->Buffer.writeFloatLE(n, ~offset=self.offset)->ignore
    self.offset = self.offset + 4
    self
}
let addInt16LE = (self: t, n: int): t => {
    self->accountForSpace(~numberOfBytesToWrite=2)
    self.current->Buffer.writeInt16LE(n->land(0xFF_FF), ~offset=self.offset)->ignore
    self.offset = self.offset + 2
    self
}
let addInt16BE = (self: t, n: int): t => {
    self->accountForSpace(~numberOfBytesToWrite=2)
    self.current->Buffer.writeInt16BE(n->land(0xFF_FF), ~offset=self.offset)->ignore
    self.offset = self.offset + 2
    self
}
let addInt32BE = (self: t, n: int): t => {
    self->accountForSpace(~numberOfBytesToWrite=4)
    self.current->Buffer.writeInt32BE(n->land(0xFF_FF_FF_FF), ~offset=self.offset)->ignore
    self.offset = self.offset + 4
    self
}
let addInt32LE = (self: t, n: int): t => {
    self->accountForSpace(~numberOfBytesToWrite=4)
    self.current->Buffer.writeInt32LE(n->land(0xFF_FF_FF_FF), ~offset=self.offset)->ignore
    self.offset = self.offset + 4
    self
}
let addDoubleLE = (self: t, n: float): t => {
    self->accountForSpace(~numberOfBytesToWrite=8)
    self.current->Buffer.writeDoubleLE(n, ~offset=self.offset)->ignore
    self.offset = self.offset + 8
    self
}
let addDoubleBE = (self: t, n: float): t => {
    self->accountForSpace(~numberOfBytesToWrite=8)
    self.current->Buffer.writeDoubleBE(n, ~offset=self.offset)->ignore
    self.offset = self.offset + 8
    self
}
let addUint32LE = (self: t, n: int): t => {
    self->accountForSpace(~numberOfBytesToWrite=4)
    self.current->Buffer.writeUint32LE(n->land(0xFF_FF_FF_FF), ~offset=self.offset)->ignore
    self.offset = self.offset + 4
    self
}
let addUint16LE = (self: t, n: int): t => {
    self->accountForSpace(~numberOfBytesToWrite=2)
    self.current->Buffer.writeUint16LE(n->land(0xFF_FF), ~offset=self.offset)->ignore
    self.offset = self.offset + 2
    self
}
let addUint16BE = (self: t, n: int): t => {
    self->accountForSpace(~numberOfBytesToWrite=2)
    self.current->Buffer.writeUint16BE(n->land(0xFF_FF), ~offset=self.offset)->ignore
    self.offset = self.offset + 2
    self
}
let addUint32BE = (self: t, n: int): t => {
    self->accountForSpace(~numberOfBytesToWrite=4)
    self.current->Buffer.writeUint32BE(n->land(0xFF_FF_FF_FF), ~offset=self.offset)->ignore
    self.offset = self.offset + 4
    self
}
let addString = (self: t, str: string): t => {
    let numberOfBytesToWrite = Buffer.byteLengthString(str)
    self->accountForSpace(~numberOfBytesToWrite)
    self.current->Buffer.writeOffset(str, ~offset=self.offset)->ignore
    self.offset = self.offset + numberOfBytesToWrite
    self
}
let data = (self: t) => {
    Array.concat(self.past, [self.current->Buffer.slice(~start=0, ~end_=self.offset)])
    ->Buffer.concat
}
