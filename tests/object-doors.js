const fs = require('node:fs');
const vm = require('node:vm');
const assert = require('node:assert/strict');
const handlers = {};
const arcs = [];
const events = {};
const context = new Proxy({}, {get: (_, key) => key === 'arc' ? (...args) => arcs.push(args) : () => {}});
const canvas = () => ({style: {}, getContext: () => context,
    addEventListener: (name, fn) => events[name] = fn,
    getBoundingClientRect: () => ({left: 0, top: 0})});
const elements = {ObjectsCanvas: canvas(), ObjectsBackground: canvas()};
const sent = [];
const sandbox = {document: {getElementById: id => elements[id] || {}, readyState: 'complete'},
    Shiny: {addCustomMessageHandler: (name, fn) => handlers[name] = fn,
        setInputValue: (_, data) => sent.push(data)}, alert: () => {}};
vm.createContext(sandbox);
vm.runInContext(fs.readFileSync('inst/Shiny/www/ObjectsCanvas.js', 'utf8'), sandbox);
const run = code => vm.runInContext(code, sandbox);
assert.equal(arcs.length, 0);
handlers.setRoomForObjects({roomName: 'Room', length: 6, width: 4, doors: [
    {x: 1.5, y: 0, clearance: {x: 1, y: 0, length: 1, width: 1}},
    {x: 6, y: 2.5, clearance: {x: 5, y: 2, length: 1, width: 1}}
]});
assert.equal(arcs.length, 2);
assert.equal(run('hasCollision({x: 1, y: 0, length: 1, width: 1})'), true);
assert.equal(run('hasCollision({x: 5, y: 2, length: 1, width: 1})'), true);
assert.equal(run('hasCollision({x: 1, y: 1, length: 1, width: 1})'), false);
handlers.addObjectToCanvas({name: 'Desk', x: 1, y: 0, length: 1, width: 1});
assert.equal(run('hasCollision(objectsArray[0], 0)'), false);
assert.equal(sent.at(-1).roomName, 'Room');
// A drag into the top door's clearance must preserve the last valid position.
run('objectsArray = [{name: "Desk", x: 1, y: 1, length: 1, width: 1}]');
events.mousedown({clientX: 60, clientY: 60});
events.mousemove({clientX: 60, clientY: 20});
assert.equal(run('objectsArray[0].y'), 1);
events.mouseup({});
// Refreshing the room removes deleted doors and cancels any stale selection.
handlers.setRoomForObjects({roomName: 'Room', length: 6, width: 4, doors: []});
assert.equal(run('hasCollision({x: 1, y: 0, length: 1, width: 1})'), false);
assert.equal(run('selectedObjectIndex'), -1);
assert.equal(run('hasCollision({x: 6, y: 0, length: 1, width: 1})'), true);
console.log('Object canvas door checks passed');
