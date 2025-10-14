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
    constructor(id, x, y, center_x, center_y, door_x, door_y, length, width, height, color, colorStroke, text, side) {
        this.id = id
        this.x = x
        this.y = y
        this.center_x = center_x
        this.center_y = center_y
        this.door_x = door_x
        this.door_y = door_y
        this.side = side
        this.type = 'rectangle';
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

        if (this.selected) {
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


        if (this.side !== '') {
          context.fillStyle = 'yellow';
          context.strokeStyle = 'yellow';

          const centerX = this.x + this.length / 2;
          const centerY = this.y + this.width / 2;

          if (this.side === 'top') {
            context.fillRect(centerX - 5, this.y - 5, 10, 10);
          } else if (this.side === 'bottom') {
            context.fillRect(centerX - 5, this.y + this.width - 5,10, 10);
          } else if (this.side === 'left') {
            context.fillRect(this.x - 5, centerY - 5, 10, 10);
          } else if (this.side === 'right') {
            context.fillRect(this.x + this.length - 5, centerY - 2.5, 10, 10);
          }
        }

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

        this.init();
    }

    init() {
        mainCanvas.addEventListener('click', e => {
          if(this.id === selectedCanvas){
            let mouse = this.getMouseCoords(e);
          }
            // Handle click on this canvas
        });
        mainCanvas.addEventListener('mousemove', e => {
            if(this.id === selectedCanvas){
                  let mouse = this.getMouseCoords(e);
                  let arr = this.arrayObject.map(e => {
                      if (e.type === 'rectangle') {
                          return this.cursorInRect(mouse.x, mouse.y, e.x, e.y, e.length, e.width);
                      } else if (e.type === 'circle') {
                          return this.cursorInCircle(mouse.x, mouse.y, e.x, e.y, e.radius);
                      }
                      return false;
                  });

                  if (!arr.every(e => e === false)) {
                      this.canvas.classList.add('pointer');
                  } else {
                      this.canvas.classList.remove('pointer');
                  }
                  this.arrayObject.forEach(e => {
                      if(e.selected && e.type === 'rectangle'){
                        if(this.isOut(e))
                          return;
                      }

                      if (e.selected) {
                          Shiny.onInputChange("type", e.type);
                          Shiny.onInputChange("id", e.id);
                          e.x = mouse.x - e.offset.x;
                          e.y = mouse.y - e.offset.y;
                          Shiny.onInputChange("x", e.x);
                          Shiny.onInputChange("y", e.y);
                      }

                      Shiny.onInputChange("selected", e.selected);

                      if (e.type === 'rectangle') {
                          if (this.cursorInRect(mouse.x, mouse.y, e.x, e.y, e.length, e.width)) {
                              e.active != true ? e.activate() : false;
                          } else {
                              e.active = false;
                          }
                      } else if (e.type === 'circle') {
                          if (this.cursorInCircle(mouse.x, mouse.y, e.x, e.y, e.radius)) {
                              e.active != true ? e.activate() : false;
                          } else {
                              e.active = false;
                          }
                      }
                  });
            }
  });
        mainCanvas.addEventListener('mousedown', e => {
          if(this.id === selectedCanvas){
            let mouse = this.getMouseCoords(e);

            if (this.type === 'rectangle'){
              this.movement_completed = false
              Shiny.onInputChange("movement_completed", this);
            }

            this.arrayObject.forEach(e => {
                if (e.type === 'rectangle' && this.cursorInRect(mouse.x, mouse.y, e.x, e.y, e.length, e.width)) {
                    e.selected = true;
                    e.offset = this.getOffsetCoords(mouse, e);
                    e.oldx = e.x
                    e.oldy = e.y
                } else if (e.type === 'circle' && this.cursorInCircle(mouse.x, mouse.y, e.x, e.y, e.radius)) {
                    e.selected = true;
                    e.offset = this.getOffsetCoords(mouse, e);
                    e.oldx = e.x
                    e.oldy = e.y
                } else {
                    e.selected = false;
                }
            })
          }
      });
        mainCanvas.addEventListener('mouseup', e => {
          let overlap = false;

          if (this.id === selectedCanvas) {
            // Loop through each object in the array
            this.arrayObject.forEach(obj => {
              // Snap to grid for all objects
              if(obj.selected){
                if(obj.type === 'circle'){
                  obj.x = Math.floor(obj.x / 10) * 10;
                  obj.y = Math.floor(obj.y / 10) * 10;
                }
                else{
                  obj.x = Math.round(obj.x / 10) * 10;
                  obj.y = Math.round(obj.y / 10) * 10;
                }
              }

              // If the object is a circle, apply the 5 offset after rounding
                if (obj.selected && obj.type === 'circle' && obj.x % 10 == 0 && obj.y % 10 == 0) {
                  obj.x += 5;
                  obj.y += 5;
                }

              // Draw object
              obj.draw(this.ctx);

              if (obj.selected && (obj.type === 'rectangle' || obj.type === 'circle')) {
                // Check for overlap
                if (this.isOverlap(obj)) {
                  if (!overlap) {
                    alert("Two objects overlap!");
                    overlap = true;
                  }
                }

                // Revert if overlap found
                if (obj.selected && overlap) {
                  obj.x = obj.oldx;
                  obj.y = obj.oldy;
                  Shiny.onInputChange("x", obj.x);
                  Shiny.onInputChange("y", obj.y);
                  Shiny.onInputChange("selected", obj.selected);
                }

                // Check if object is out of bounds
                if (this.isOut(obj)) return;
              }

              // Deselect after processing
              if (obj.selected && obj.type === 'rectangle'){
                obj.movement_completed = true;
                Shiny.onInputChange("movement_completed", obj);
              }
              obj.selected = false;
            });

            // If no overlap, filter out 'segment' objects
            if (!overlap) {
              this.arrayObject = this.arrayObject.filter(obj => obj.type !== 'segment');
            }
          }
        });

        // Add other event listeners and initialization logic as needed
        this.animate();
    }

    getMouseCoords(event) {
        let canvasCoords = this.canvas.getBoundingClientRect();
        return {
            x: event.clientX - canvasCoords.left,
            y: event.clientY - canvasCoords.top
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
            if(this.arrayObject[i].type === 'rectangle')
            {
              const rect = this.arrayObject[i];
              if ((event.id != rect.id)){
                if (event.x + event.length > rect.x - 10 && event.x < rect.x + rect.length + 10 &&
                    event.y + event.width > rect.y - 10 && event.y < rect.y + rect.width + 10){
                  overlap = true;  // C'è sovrapposizione
                }
              }
            }

            if(this.arrayObject[i].type === 'circle')
            {
              const circle = this.arrayObject[i];
              if(circle.x >= event.x && circle.x <= event.x + event.length + 10 &&
                 circle.y >= event.y && circle.y <= event.y + event.width+ 10){
                overlap = true;  // C'è sovrapposizione
              }
            }
          }
          else{
            if(this.arrayObject[i].type === 'rectangle')
            {
              const rect = this.arrayObject[i];
              if (event.x >= rect.x && event.x <= rect.x + rect.length + 10 &&
                  event.y >= rect.y && event.y <= rect.y + rect.width+ 10){
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

    animate() {
        this.ctx.clearRect(0, 0, w, h);
        this.arrayObject.forEach(obj => obj.draw(this.ctx));
        window.requestAnimationFrame(() => this.animate());
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
