let h = h_base = 800;
let w = w_base = 1000;
let selectedCanvas = "";
// =============================================================
//                     Main Canvas DEFINITION
// =============================================================
// set the canvas in which the rooms are drawn

let canvasContainer = document.getElementById("canvasContainer");

let mainCanvas = document.getElementById('MainCanvas')
let mainCtx = mainCanvas.getContext('2d')
mainCanvas.style.backgroundColor = 'trasparent'
mainCanvas.width = w_base;
mainCanvas.height = h_base;

// =============================================================
//                     background DEFINITION
// =============================================================
// set the background with the grid, which is unique and it is not deleted every canvas changes
// function to draw the grid
// react to Shiny input changes
// It takes in input the image dimensions and put it in the center of the canvas

Shiny.addCustomMessageHandler("bgImageChanged", msg => {
  const { imgFile, wBG, hBG } = msg;

  if (!imgFile) {
    ctx.clearRect(0, 0, w, h);
    drawBG(ctx);
    return;
  }

  console.log('Image selected:', imgFile);
  const img = new Image();
  img.src = imgFile;            // Shiny will serve www/img.png as "/img.png"
  img.onload = () => {
    // clear and paint the image

    ctx.clearRect(0, 0, w, h);
    const scaledW = wBG;  // your background image width
    const scaledH = hBG;  // your background image height
    const offsetX = (w - scaledW) / 2;
    const offsetY = (h - scaledH) / 2;

    ctx.drawImage(img, offsetX, offsetY, scaledW, scaledH);
    // now draw the grid on top
    drawBG(ctx);
  };
});

function drawBG(context) {

    context.save()

    //context.fillStyle = 'trasparent'
    //context.fillRect(0, 0, w, h)
    context.lineWidth = 0.3;
    context.strokeStyle = 'lightgray'
    //context.fillStyle = 'black'

    for (let i = 1; i < w; i++) {
        context.beginPath()
        if (i % 10 === 0) {
            context.moveTo(i, 0);
            context.lineTo(i, h)
            context.moveTo(i, 0);
        }
        context.closePath()
        context.stroke()
    }

    for (let i = 1; i < h; i++) {
        context.beginPath()
        if (i % 10 === 0) {
            context.moveTo(0, i)
            context.lineTo(w, i)
            context.moveTo(0, i)
        }
        context.closePath()
        context.stroke()
    }

    context.lineWidth = 1
    context.strokeStyle = 'gray'

    context.beginPath()
    for (let i = 50; i < w; i += 10) {
        if (i % 50 === 0) {
            context.moveTo(i, 0)
            context.lineTo(i, 30)
            context.fillText(` ${i/ 10} m`, i, 30)
        } else {
            context.moveTo(i, 0)
            context.lineTo(i, 10)
        }

    }
    context.closePath()
    context.stroke()

    context.beginPath()
    for (let i = 50; i < h; i += 10) {
        if (i % 50 === 0) {
            context.moveTo(0, i)
            context.lineTo(30, i)
            context.fillText(` ${i/ 10} m`, 30, i)
        } else {
            context.moveTo(0, i)
            context.lineTo(10, i)
        }
    }
    context.closePath()
    context.stroke()

    context.restore()
}

let background = document.getElementById('Background')
let ctx = background.getContext('2d')
background.style.backgroundColor = 'trasparent'
background.width = w_base;
background.height = h_base;

ctx.lineWidth = 2
ctx.textAlign = 'center'
ctx.textBaseline = 'middle'
ctx.font = '10px Arial'
// drawBG(ctx)

// =============================================================

let rgba = (r, g, b, a) => `rgba(${r},${g},${b},${a})`

let drawCoords = (ctx, x, y, color = "green") => {
    ctx.save()
    ctx.translate(x, y)
    ctx.fillStyle = color
    ctx.fillRect(-45, -7, 30, 14)
    ctx.fillStyle = 'white'
    ctx.fillText(Math.floor(x), -30, 0)
    ctx.rotate(Math.PI / 2)
    ctx.fillStyle = color
    ctx.fillRect(-45, -7, 30, 14)
    ctx.fillStyle = 'white'
    ctx.fillText(Math.floor(y), -30, 0)
    ctx.restore()
}

// =============================================================
//                     CLASS DEFINITION
// =============================================================

// Initialize Floor Array
let FloorArray = {};

class Room {
    constructor(id, x, y, center_x, center_y, length, width, height, color, colorStroke, text, doors = [], roomType = 'Normal', zIndex = 0) {
        this.id = id
        this.x = x
        this.y = y
        this.center_x = center_x
        this.center_y = center_y
        this.doors = doors
        this.roomType = roomType
        this.zIndex = zIndex
        this.focused = false
        this.type = 'rectangle';
        // Pixel span between wall-cell centres: (interior dimension + 1) * 10.
        // The wall band itself is not painted on the canvas.
        this.length = length;
        this.width = width;
        this.height = height;
        this.color = color
        this.colorStroke = colorStroke
        this.selected = false
        this.active = false
        this.movement_completed = false
        this.activeColor = color.replace(/,\d\d%\)/, str => str.replace(/\d\d/, str.match(/\d\d/)[0] * 0.7))
        this.activeColor2 = color.replace(/,\d\d%\)/, str => str.replace(/\d\d/, str.match(/\d\d/)[0] * 0.6))
        this.text = text; // Aggiungi la proprietà del testo
    }
    draw(context) {
        context.fillStyle = this.color
        if (this.active) {
            context.fillStyle = this.activeColor;
            context.save()
            context.setLineDash([10, 5, 30, 5])
            context.beginPath()
            context.moveTo(this.x, this.y)
            context.lineTo(0, this.y)
            context.moveTo(this.x, this.y)
            context.lineTo(this.x, 0)
            context.moveTo(this.x, this.y)
            context.closePath()
            context.lineWidth = 0.5
            context.strokeStyle = this.activeColor
            context.stroke()

            drawCoords(context, this.x/10, this.y/10, this.activeColor)

            context.restore()
        }

        // Codice JavaScript
        //console.log('Valore di x:', this.x);
        //console.log('Valore di y:', this.y);
        context.fillRect(this.x, this.y, this.length, this.width);

        //Imposta lo stile del bordo
        context.lineWidth = 2;
        context.strokeStyle = this.colorStroke; //  il colore desiderato per il bordo
        //Disegna il rettangolo con il bordo colorato
        context.strokeRect(this.x, this.y, this.length, this.width);

        if (this.selected || this.focused) {
            context.lineWidth = 2;
            context.strokeStyle = this.activeColor2;
            context.strokeRect(this.x, this.y, this.length, this.width);
        }

        // Disegna il testo al centro del rettangolo
        context.fillStyle = "white";
        context.textAlign = "center";
        context.textBaseline = "middle";
        context.font = "12px sans-serif";
        context.fillText(this.text + "\n #" + this.id, this.x + this.length / 2, this.y + this.width / 2);


    }

    drawDoors(context, visible = () => true) {
        this.doors.filter(visible).forEach(door => {
            const position = this.doorPosition(door);
            context.fillStyle = 'yellow';
            context.fillRect(position.x - 5, position.y - 5, 10, 10);
            context.strokeStyle = '#333';
            context.lineWidth = 1;
            context.strokeRect(position.x - 5, position.y - 5, 10, 10);
        });
    }

    doorPosition(door) {
        if (door.side === 'interior') {
            return {x: this.x + door.local_x * 10, y: this.y + door.local_y * 10};
        }
        const horizontal = door.side === 'top' || door.side === 'bottom';
        return {
            x: this.x + (horizontal ? door.offset * 10 : (door.side === 'right' ? this.length : 0)),
            y: this.y + (!horizontal ? door.offset * 10 : (door.side === 'bottom' ? this.width : 0))
        };
    }

    doorAt(mouse, remove = false) {
        if (this.roomType === 'Fillingroom') return null;
        if (remove) {
            return this.doors.find(door => {
                const p = this.doorPosition(door);
                return Math.abs(mouse.x - p.x) <= 5 && Math.abs(mouse.y - p.y) <= 5;
            }) || null;
        }
        const dx = mouse.x - this.x;
        const dy = mouse.y - this.y;
        const walls = [
            {side: 'top', distance: Math.abs(dy), along: dx, length: this.length},
            {side: 'bottom', distance: Math.abs(dy - this.width), along: dx, length: this.length},
            {side: 'left', distance: Math.abs(dx), along: dy, length: this.width},
            {side: 'right', distance: Math.abs(dx - this.length), along: dy, length: this.width}
        ].filter(wall => wall.distance <= 5 && Math.round(wall.along / 10) >= 1 &&
            Math.round(wall.along / 10) <= wall.length / 10 - 1)
         .sort((a, b) => a.distance - b.distance);
        if (!walls.length) return null;
        return {side: walls[0].side, offset: Math.round(walls[0].along / 10)};
    }

    update() {
        this.x += 0.1
    }

    select() {
        this.selected = !this.selected
    }

    activate() {
        this.active = !this.active
    }
}

class Circle {
    constructor(id, x, y, radius, color) {
        this.id = id;
        this.x = x;
        this.y = y;
        this.type = 'circle';
        this.radius = radius;
        this.color = color;
        this.rotation = 0;  // Aggiungi la proprietà per la rotazione in gradi
        this.selected = false;
        this.active = false;
        this.activeColor = color.replace(/,\d\d%\)/, str => str.replace(/\d\d/, str.match(/\d\d/)[0] * 0.7));
        this.activeColor2 = color.replace(/,\d\d%\)/, str => str.replace(/\d\d/, str.match(/\d\d/)[0] * 0.6));
    }

    draw(context) {
        context.fillStyle = this.color;

        if (this.active) {
            context.fillStyle = this.activeColor;
            context.save()
            context.setLineDash([10, 5, 30, 5]);
            context.beginPath();
            context.arc(this.x, this.y,  this.radius, 0, 2 * Math.PI);
            context.closePath();
            context.lineWidth = 0.5;
            context.strokeStyle = this.activeColor;
            context.stroke();
            drawCoords(context, 0, 0, this.activeColor);
            context.restore();  // Ripristina lo stato del contesto
        }

        context.beginPath();
        context.arc(this.x, this.y, this.radius, 0, 2 * Math.PI);
        context.closePath();
        context.fill();

        if (this.selected) {
            context.lineWidth = 2;
            context.strokeStyle = this.activeColor2;
            context.stroke();
        }
    }

    update() {
        this.x += 0.1;
    }

    select() {
        this.selected = !this.selected;
    }

    activate() {
        this.active = !this.active;
    }

}

class Segment {
    constructor(id, x1, y1, x2, y2) {
        this.id = id;
        this.x1 = x1;
        this.y1 = y1;
        this.x2 = x2;
        this.y2 = y2;
        this.type = 'segment';
    }

    draw(context) {
        context.beginPath();
        context.moveTo(this.x1, this.y1);
        context.lineTo(this.x2, this.y2);
        context.stroke();
        context.strokeStyle = 'red';
    }
}

class FloorManager {
    constructor(floorId) {
        this.id = floorId;
        this.canvas = mainCanvas;
        this.ctx = mainCtx;
        this.w = w_base;
        this.h = h_base;
        this.arrayObject = [];
        this.pendingMovement = false;
        this.animating = false;

        this.init();
    }

    isCurrent() {
        return this.id === selectedCanvas && FloorArray[this.id] === this;
    }

    tool() {
        const selected = document.querySelector('input[name="canvas_tool"]:checked');
        return selected ? selected.value : 'move';
    }

    orderedRooms() {
        return this.arrayObject.filter(obj => obj.type === 'rectangle')
            .sort((a, b) => a.zIndex - b.zIndex || a.id - b.id);
    }

    orderedObjects() {
        return [...this.orderedRooms(), ...this.arrayObject.filter(obj => obj.type !== 'rectangle')];
    }

    hitObject(mouse) {
        return [...this.orderedObjects()].reverse().find(obj =>
            obj.type === 'rectangle'
                ? this.cursorInRect(mouse.x, mouse.y, obj.x, obj.y, obj.length, obj.width)
                : obj.type === 'circle' && this.cursorInCircle(mouse.x, mouse.y, obj.x, obj.y, obj.radius));
    }

    doorVisible(room, door) {
        const p = room.doorPosition(door);
        const rooms = this.orderedRooms();
        return !rooms.slice(rooms.indexOf(room) + 1).some(other =>
            this.cursorInRect(p.x, p.y, other.x, other.y, other.length, other.width));
    }

    doorTarget(mouse) {
        if (this.tool() === 'remove_door') {
            for (const room of this.orderedRooms().reverse()) {
                const door = room.doorAt(mouse, true);
                if (door && this.doorVisible(room, door)) return {room, door};
            }
            return null;
        }
        for (const room of this.orderedRooms().reverse()) {
            const door = room.doorAt(mouse);
            if (door && this.doorVisible(room, door)) return {room, door};
            if (this.cursorInRect(mouse.x, mouse.y, room.x, room.y, room.length, room.width)) return null;
        }
        return null;
    }

    init() {
        mainCanvas.addEventListener('click', event => {
            if (!this.isCurrent() || this.pendingMovement || this.tool() === 'move') return;
            const target = this.doorTarget(this.getMouseCoords(event));
            if (!target) return;
            Shiny.setInputValue('canvas_door_click', {
                CanvasID: this.id, roomID: target.room.id,
                doorID: target.door.ID, side: target.door.side,
                offset: target.door.offset, action: this.tool()
            }, {priority: 'event'});
        });
        mainCanvas.addEventListener('mousemove', event => {
            if (!this.isCurrent() || this.pendingMovement) return;
            const mouse = this.getMouseCoords(event);
            if (this.tool() !== 'move') {
                this.canvas.classList.toggle('pointer', !!this.doorTarget(mouse));
                return;
            }
            const hovered = this.hitObject(mouse);
            this.arrayObject.forEach(obj => {
                if (obj.selected) {
                    obj.x = mouse.x - obj.offset.x;
                    obj.y = mouse.y - obj.offset.y;
                    this.isOut(obj);
                }
                obj.active = obj === hovered;
            });
            this.canvas.classList.toggle('pointer', !!hovered);
        });
        mainCanvas.addEventListener('mousedown', event => {
            if (!this.isCurrent() || this.pendingMovement || this.tool() !== 'move' || event.button !== 0) return;
            const mouse = this.getMouseCoords(event);
            const target = this.hitObject(mouse);
            this.arrayObject.forEach(obj => {
                obj.selected = obj === target;
                obj.focused = obj === target;
                if (obj.selected) {
                    obj.offset = this.getOffsetCoords(mouse, obj);
                    obj.oldx = obj.x;
                    obj.oldy = obj.y;
                }
            });
            if (target && target.type === 'rectangle') {
                Shiny.setInputValue('canvas_room_selected', {
                    CanvasID: this.id, roomID: target.id
                }, {priority: 'event'});
            }
        });
        // Complete a drag even when the pointer is released outside the canvas.
        window.addEventListener('mouseup', () => {
            if (!this.isCurrent()) return;
            this.arrayObject.forEach(obj => {
                if (!obj.selected) return;
                if (obj.type === 'circle') {
                    obj.x = Math.floor(obj.x / 10) * 10 + 5;
                    obj.y = Math.floor(obj.y / 10) * 10 + 5;
                } else {
                    obj.x = Math.round(obj.x / 10) * 10;
                    obj.y = Math.round(obj.y / 10) * 10;
                }
                this.isOut(obj);
                if (this.isOverlap(obj)) {
                    obj.x = obj.oldx;
                    obj.y = obj.oldy;
                    alert('Two objects overlap!');
                }
                obj.selected = false;
                if (obj.x !== obj.oldx || obj.y !== obj.oldy) {
                    if (obj.type === 'rectangle') this.pendingMovement = true;
                    Shiny.setInputValue('canvas_object_moved', {
                        CanvasID: this.id, type: obj.type, id: obj.id, x: obj.x, y: obj.y
                    }, {priority: 'event'});
                    if (obj.type === 'circle') {
                        this.arrayObject = this.arrayObject.filter(item => item.type !== 'segment');
                    }
                }
            });
        });
    }

    getMouseCoords(event) {
        // Account for CSS scaling, scrolling and the canvas border.
        const bounds = this.canvas.getBoundingClientRect();
        const scaleX = this.canvas.offsetWidth / bounds.width;
        const scaleY = this.canvas.offsetHeight / bounds.height;
        return {
            x: ((event.clientX - bounds.left) * scaleX - this.canvas.clientLeft) * this.canvas.width / this.canvas.clientWidth,
            y: ((event.clientY - bounds.top) * scaleY - this.canvas.clientTop) * this.canvas.height / this.canvas.clientHeight
        };
    }

    getOffsetCoords = (mouse, rect) => {
    return {
        x: mouse.x - rect.x,
        y: mouse.y - rect.y
    }
}

    cursorInRect = (mouseX, mouseY, rectX, rectY, rectW, rectH) => {
        let xLine = mouseX > rectX && mouseX < rectX + rectW
        let yLine = mouseY > rectY && mouseY < rectY + rectH

        return xLine && yLine
    }

    cursorInCircle = (mouseX, mouseY, circX, circY,circR) => {
        // Calcola la distanza tra il centro del cerchio e le coordinate del mouse
        const distance = Math.sqrt((mouseX - circX) ** 2 + (mouseY - circY) ** 2);

        // Verifica se la distanza è inferiore al raggio del cerchio
        return distance <= circR;
    }

    isOverlap(event) {
        let overlap = false;
        for (let i = 0; i < this.arrayObject.length; i++) {
          if(event.type === 'rectangle'){
            // Rooms may contain or overlap other rooms. Point collisions retain
            // their existing behaviour.
            if(this.arrayObject[i].type === 'circle')
            {
              const circle = this.arrayObject[i];
              if(circle.x >= event.x && circle.x <= event.x + event.length + 10 &&
                 circle.y >= event.y && circle.y <= event.y + event.width + 10){
                overlap = true;  // C'è sovrapposizione
              }
            }
          }
          else{
            if(this.arrayObject[i].type === 'rectangle')
            {
              const rect = this.arrayObject[i];
              if (event.x >= rect.x && event.x <= rect.x + rect.length + 10 &&
                  event.y >= rect.y && event.y <= rect.y + rect.width + 10){
                overlap = true;  // C'è sovrapposizione
              }
            }

            if(this.arrayObject[i].type === 'circle')
            {
              const circle = this.arrayObject[i];
              if ((event.id != circle.id)){
                if(circle.x == event.x && circle.y == event.y){
                  overlap = true;  // C'è sovrapposizione
                }
              }
            }
          }
        }

        return overlap;  // Nessuna sovrapposizione
    }

    isOut(event){
        let out = false;
        if (event.x + event.length > this.canvas.width - 10){
          event.x = this.canvas.width - event.length - 10;
          out = true;   // E' fuori dal rettangolo
        }
        if(event.y + event.width > this.canvas.height - 10){
          event.y = this.canvas.height - event.width - 10;
          out = true;   // E' fuori dal rettangolo
        }
        if(event.x < 10){
          event.x = 10;
          out = true;   // E' fuori dal rettangolo
        }
        if(event.y < 10){
          event.y = 10;
          out = true;   // E' fuori dal rettangolo
        }
        return out;
    }

    draw() {
        this.ctx.clearRect(0, 0, w, h);
        this.orderedObjects().forEach(obj => obj.draw(this.ctx));
        this.orderedRooms().forEach(room =>
            room.drawDoors(this.ctx, door => this.doorVisible(room, door)));
    }

    animate() {
        if (this.animating) return;
        this.animating = true;
        const frame = () => {
            if (!this.isCurrent()) {
                this.animating = false;
                return;
            }
            this.draw();
            window.requestAnimationFrame(frame);
        };
        frame();
    }

}

// =============================================================

// Function to add a new floor
function addFloor(floorId) {
    FloorArray[floorId] = new FloorManager(floorId);
}

let arr = new Array(40).fill('empty').map(() => Math.floor(Math.random() * 100))

// =============================================================
//                          MAIN LOOP
// =============================================================


// Handle canvas selection change
$('#canvas_selector').on('change', function () {

  selectedCanvas = $(this).val();

  if( selectedCanvas != ""){
    console.log('Selected canvas:', selectedCanvas);

    let selectedFloor = FloorArray[selectedCanvas]

    if(!selectedFloor){
      console.log('Adding a new canvas:',selectedCanvas);
      addFloor(selectedCanvas);
      selectedFloor = FloorArray[selectedCanvas]
    }

    // the first time a floor is added the BG is drawn
    console.log('length:', Object.keys(FloorArray).length);
    if(Object.keys(FloorArray).length == 1){
      background.style.backgroundColor = "white";
      drawBG(ctx);
    }
    selectedFloor.animate()
  }
  else{
    if(Object.keys(FloorArray).length == 0){
      mainCtx.clearRect(0, 0, w, h);
      ctx.clearRect(0, 0, w, h);
    }
  }
});

// Initial canvas selection
$('#canvas_selector').trigger('change');

Shiny.addCustomMessageHandler('roomDoorsChanged', message => {
    const floor = FloorArray[message.CanvasID];
    if (!floor) return;
    const room = floor.arrayObject.find(obj => obj.type === 'rectangle' && obj.id === message.roomID);
    if (room) {
        room.doors = message.doors || [];
        if (Number.isFinite(message.center_x)) room.center_x = message.center_x;
        if (Number.isFinite(message.center_y)) room.center_y = message.center_y;
    }
});

Shiny.addCustomMessageHandler('roomMoveResolved', message => {
    const floor = FloorArray[message.CanvasID];
    if (!floor) return;
    floor.pendingMovement = false;
    const room = floor.arrayObject.find(obj => obj.type === 'rectangle' && obj.id === message.id);
    if (room) {
        room.x = message.x;
        room.y = message.y;
        room.center_x = message.center_x;
        room.center_y = message.center_y;
        room.selected = false;
    }
});

Shiny.addCustomMessageHandler('invalidateCanvasPaths', message => {
    const floor = FloorArray[message.CanvasID];
    if (floor) floor.arrayObject = floor.arrayObject.filter(obj => obj.type !== 'segment');
});

Shiny.addCustomMessageHandler('roomLayersChanged', message => {
    const floor = FloorArray[message.CanvasID];
    if (!floor) return;
    for (const entry of message.rooms || []) {
        const room = floor.arrayObject.find(obj => obj.type === 'rectangle' && obj.id === entry.id);
        if (room) room.zIndex = entry.z_index;
    }
});

Shiny.addCustomMessageHandler('canvasRoomSelected', message => {
    const floor = FloorArray[message.CanvasID];
    if (!floor) return;
    floor.arrayObject.forEach(obj => {
        obj.focused = obj.type === 'rectangle' && obj.id === message.roomID;
    });
});
