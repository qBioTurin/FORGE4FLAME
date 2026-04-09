// =============================================================
//                     Objects Canvas DEFINITION
// =============================================================
// Canvas for drawing objects within a selected room

let obj_h = obj_h_base = 800;
let obj_w = obj_w_base = 1000;
let objectsArray = [];
let selectedObjectIndex = -1;
let isDragging = false;
let dragStartX = 0;
let dragStartY = 0;
let currentRoomData = null;

// Scale factor: 40 pixels = 1 meter (20 pixels = 50 cm)
const SCALE = 40;

// =============================================================
//                     Objects Canvas DEFINITION
// =============================================================
let objectsCanvasContainer = document.getElementById("objectsCanvasContainer");

let objectsCanvas = document.getElementById('ObjectsCanvas');
let objectsCtx = objectsCanvas.getContext('2d');
objectsCanvas.style.backgroundColor = 'transparent';
objectsCanvas.width = obj_w_base;
objectsCanvas.height = obj_h_base;

// =============================================================
//                     Background Canvas DEFINITION
// =============================================================
let objectsBgCanvas = document.getElementById('ObjectsBackground');
let objectsBgCtx = objectsBgCanvas.getContext('2d');
objectsBgCanvas.width = obj_w_base;
objectsBgCanvas.height = obj_h_base;

// Initialize canvas with empty grid
function initObjectsCanvas() {
    obj_w = objectsCanvas.width = objectsBgCanvas.width;
    obj_h = objectsCanvas.height = objectsBgCanvas.height;
    drawObjectsGrid();
}

// Draw grid on background canvas
function drawObjectsGrid() {
    objectsBgCtx.clearRect(0, 0, obj_w, obj_h);

    objectsBgCtx.save();
    objectsBgCtx.lineWidth = 0.3;
    objectsBgCtx.strokeStyle = 'lightgray';

    // Vertical lines (every 40 pixels = 1 m)
    for (let i = 1; i < obj_w; i++) {
        objectsBgCtx.beginPath();
        if (i % 40 === 0) {
            objectsBgCtx.moveTo(i, 0);
            objectsBgCtx.lineTo(i, obj_h);
            objectsBgCtx.moveTo(i, 0);
        }
        objectsBgCtx.closePath();
        objectsBgCtx.stroke();
    }

    // Horizontal lines (every 40 pixels = 1 m)
    for (let i = 1; i < obj_h; i++) {
        objectsBgCtx.beginPath();
        if (i % 40 === 0) {
            objectsBgCtx.moveTo(0, i);
            objectsBgCtx.lineTo(obj_w, i);
            objectsBgCtx.moveTo(0, i);
        }
        objectsBgCtx.closePath();
        objectsBgCtx.stroke();
    }

    objectsBgCtx.lineWidth = 1;
    objectsBgCtx.strokeStyle = 'gray';

    objectsBgCtx.beginPath();
    // X-axis labels (every 1 meter = 40 pixels)
    for (let i = 40; i < obj_w; i += 40) {
        objectsBgCtx.moveTo(i, 0);
        objectsBgCtx.lineTo(i, 30);
        objectsBgCtx.fillText(` ${i / 40} m`, i, 30);
    }
    objectsBgCtx.closePath();
    objectsBgCtx.stroke();

    objectsBgCtx.beginPath();
    // Y-axis labels (every 1 meter = 40 pixels)
    for (let i = 40; i < obj_h; i += 40) {
        objectsBgCtx.moveTo(0, i);
        objectsBgCtx.lineTo(30, i);
        objectsBgCtx.fillText(` ${i / 40} m`, 0, i);
    }
    objectsBgCtx.closePath();
    objectsBgCtx.stroke();

    objectsBgCtx.restore();
}

// Set room dimensions and update canvas
Shiny.addCustomMessageHandler("setRoomForObjects", function (data) {
    currentRoomData = data;
    objectsArray = data.objects || [];

    // Get room dimensions in meters
    const roomWidth = data.width || 10;   // width in meters
    const roomLength = data.length || 10; // length in meters

    // Calculate canvas size: each meter = 40 pixels (2 cells of 20px each)
    // So for 4 meters, we get 4 * 40 = 160 pixels (8 cells of 20px)
    obj_w = objectsCanvas.width = objectsBgCanvas.width = roomWidth * SCALE;
    obj_h = objectsCanvas.height = objectsBgCanvas.height = roomLength * SCALE;

    // Redraw grid with new dimensions
    drawObjectsGrid();
    redrawObjectsCanvas();
});

// Draw all objects on canvas
function redrawObjectsCanvas() {
    // Clear canvas
    objectsCtx.clearRect(0, 0, objectsCanvas.width, objectsCanvas.height);

    // Draw all objects
    objectsArray.forEach((obj, index) => {
        drawObject(obj, index === selectedObjectIndex);
    });
}

// Draw a single object
function drawObject(obj, isSelected) {
    const x = obj.x * SCALE;
    const y = obj.y * SCALE;
    const width = obj.width * SCALE;
    const length = obj.length * SCALE;

    // Draw object rectangle with different style for obstacles vs. usable objects
    if (obj.isObstacle) {
        // Obstacles: darker background with diagonal stripes pattern
        objectsCtx.fillStyle = obj.color;
        objectsCtx.fillRect(x, y, width, length);

        // Add diagonal stripes for obstacles
        objectsCtx.save();
        objectsCtx.strokeStyle = 'rgba(0,0,0,0.2)';
        objectsCtx.lineWidth = 2;
        const stripeSpacing = 10;
        for (let i = -length; i < width; i += stripeSpacing) {
            objectsCtx.beginPath();
            objectsCtx.moveTo(x + i, y);
            objectsCtx.lineTo(x + i + length, y + length);
            objectsCtx.stroke();
        }
        objectsCtx.restore();
    } else {
        // Usable objects: solid color
        objectsCtx.fillStyle = obj.color;
        objectsCtx.fillRect(x, y, width, length);
    }

    // Draw border
    objectsCtx.strokeStyle = isSelected ? '#0066cc' : '#333';
    objectsCtx.lineWidth = isSelected ? 3 : 1;
    objectsCtx.strokeRect(x, y, width, length);

    // Draw object name
    objectsCtx.fillStyle = '#000';
    objectsCtx.font = 'bold 12px Arial';
    objectsCtx.textAlign = 'center';
    objectsCtx.fillText(obj.name, x + width / 2, y + length / 2);

    // Draw capacity for non-obstacles
    if (!obj.isObstacle && obj.capacity) {
        objectsCtx.font = '10px Arial';
        objectsCtx.fillStyle = '#666';
        objectsCtx.fillText('Cap: ' + obj.capacity, x + width / 2, y + length / 2 + 14);
    }
}

// Get door clear zone (1m x 1m centered on the wall)
function getDoorClearZone() {
    if (!currentRoomData || !currentRoomData.door || currentRoomData.door === 'none') {
        return null;
    }

    const roomWidth = currentRoomData.width;
    const roomLength = currentRoomData.length;

    switch (currentRoomData.door) {
        case 'left':
            return { x: 0, y: Math.floor(roomLength / 2), width: 1, length: 1 };
        case 'right':
            return { x: roomWidth - 1, y: Math.floor(roomLength / 2), width: 1, length: 1 };
        case 'top':
            return { x: Math.floor(roomWidth / 2), y: 0, width: 1, length: 1 };
        case 'bottom':
            return { x: Math.floor(roomWidth / 2), y: roomLength - 1, width: 1, length: 1 };
        default:
            return null;
    }
}

// Draw door indicator
function drawDoor() {
    const zone = getDoorClearZone();
    if (!zone) return;

    objectsCtx.save();
    objectsCtx.fillStyle = 'rgba(255, 165, 0, 0.3)'; // Orange semi-transparent
    objectsCtx.strokeStyle = '#ff8c00';
    objectsCtx.lineWidth = 2;

    const x = zone.x * SCALE;
    const y = zone.y * SCALE;
    const w = zone.width * SCALE;
    const l = zone.length * SCALE;

    objectsCtx.fillRect(x, y, w, l);
    objectsCtx.strokeRect(x, y, w, l);

    // Door Label
    objectsCtx.fillStyle = '#cc7a00';
    objectsCtx.font = 'bold 11px Arial';
    objectsCtx.textAlign = 'center';
    objectsCtx.fillText('DOOR', x + w / 2, y + l / 2 + 4);

    objectsCtx.restore();
}

// Check if two objects overlap
function checkOverlap(obj1, obj2) {
    // Check if rectangles overlap
    return !(obj1.x + obj1.width <= obj2.x ||  // obj1 is to the left of obj2
        obj2.x + obj2.width <= obj1.x ||  // obj2 is to the left of obj1
        obj1.y + obj1.length <= obj2.y || // obj1 is above obj2
        obj2.y + obj2.length <= obj1.y);  // obj2 is above obj1
}

// Check if an object overlaps with any existing objects
function hasCollision(newObj, excludeIndex = -1) {
    // Check for collisions with other objects
    for (let i = 0; i < objectsArray.length; i++) {
        if (i === excludeIndex) continue; // Skip the object being moved
        if (checkOverlap(newObj, objectsArray[i])) {
            return true;
        }
    }

    // Check for collisions with door clear zone
    const doorZone = getDoorClearZone();
    if (doorZone && checkOverlap(newObj, doorZone)) {
        return true;
    }

    return false;
}

// Draw all objects and door on canvas
function redrawObjectsCanvas() {
    // Clear canvas
    objectsCtx.clearRect(0, 0, objectsCanvas.width, objectsCanvas.height);

    // Draw door indicator first (lowest layer)
    drawDoor();

    // Draw all objects
    objectsArray.forEach((obj, index) => {
        drawObject(obj, index === selectedObjectIndex);
    });
}

// Find a non-overlapping position for a new object
function findNonOverlappingPosition(obj) {
    // Try the default position first
    if (!hasCollision(obj)) {
        return { x: obj.x, y: obj.y };
    }

    // Try positions in a grid pattern
    const step = 1; // Move in 1 meter increments
    const maxX = (objectsCanvas.width / SCALE) - obj.width;
    const maxY = (objectsCanvas.height / SCALE) - obj.length;

    for (let y = 1; y <= maxY; y += step) {
        for (let x = 1; x <= maxX; x += step) {
            const testObj = { ...obj, x: x, y: y };
            if (!hasCollision(testObj)) {
                return { x: x, y: y };
            }
        }
    }

    // No free position found
    return null;
}

// Add object to canvas
Shiny.addCustomMessageHandler("addObjectToCanvas", function (data) {
    const newObject = {
        name: data.name,
        id: data.id,
        x: data.x || 1.0,
        y: data.y || 1.0,
        width: data.width || 1,
        length: data.length || 1,
        color: data.color || '#FF6B6B',
        isObstacle: data.isObstacle || false,
        capacity: data.capacity || null
    };

    // Find a non-overlapping position
    const position = findNonOverlappingPosition(newObject);

    if (position === null) {
        // No free space available
        alert('Cannot add object: No free space available in the room. Please remove some objects first.');
        return;
    }

    // Update object position
    newObject.x = position.x;
    newObject.y = position.y;

    objectsArray.push(newObject);
    redrawObjectsCanvas();

    // Send updated objects back to Shiny
    Shiny.setInputValue('objects_updated', {
        objects: objectsArray,
        timestamp: Date.now()
    });
});

// Remove object from canvas
Shiny.addCustomMessageHandler("removeObjectFromCanvas", function (index) {
    if (index >= 0 && index < objectsArray.length) {
        objectsArray.splice(index, 1);
        selectedObjectIndex = -1;
        redrawObjectsCanvas();

        Shiny.setInputValue('objects_updated', {
            objects: objectsArray,
            timestamp: Date.now()
        });
    }
});

// Clear all objects
Shiny.addCustomMessageHandler("clearAllObjects", function (data) {
    objectsArray = [];
    selectedObjectIndex = -1;
    redrawObjectsCanvas();

    Shiny.setInputValue('objects_updated', {
        objects: objectsArray,
        timestamp: Date.now()
    });
});

// Mouse event handlers for canvas interaction
objectsCanvas.addEventListener('mousedown', function (e) {
    const rect = objectsCanvas.getBoundingClientRect();
    const mouseX = (e.clientX - rect.left) / SCALE;
    const mouseY = (e.clientY - rect.top) / SCALE;

    // Check if clicking on an existing object
    selectedObjectIndex = -1;
    for (let i = objectsArray.length - 1; i >= 0; i--) {
        const obj = objectsArray[i];
        if (mouseX >= obj.x && mouseX <= obj.x + obj.width &&
            mouseY >= obj.y && mouseY <= obj.y + obj.length) {
            selectedObjectIndex = i;
            isDragging = true;
            dragStartX = mouseX - obj.x;
            dragStartY = mouseY - obj.y;
            break;
        }
    }

    redrawObjectsCanvas();

    // Send selected object info to Shiny
    if (selectedObjectIndex >= 0) {
        Shiny.setInputValue('selected_object_index', selectedObjectIndex);
    }
});

objectsCanvas.addEventListener('mousemove', function (e) {
    if (isDragging && selectedObjectIndex >= 0) {
        const rect = objectsCanvas.getBoundingClientRect();
        const mouseX = (e.clientX - rect.left) / SCALE;
        const mouseY = (e.clientY - rect.top) / SCALE;

        const obj = objectsArray[selectedObjectIndex];
        const oldX = obj.x;
        const oldY = obj.y;

        // Calculate new position
        const newX = Math.max(0, Math.min(mouseX - dragStartX, objectsCanvas.width / SCALE - obj.width));
        const newY = Math.max(0, Math.min(mouseY - dragStartY, objectsCanvas.height / SCALE - obj.length));

        // Temporarily update position
        obj.x = newX;
        obj.y = newY;

        // Check for collision with other objects
        if (hasCollision(obj, selectedObjectIndex)) {
            // Collision detected, revert to old position
            obj.x = oldX;
            obj.y = oldY;
        }

        redrawObjectsCanvas();
    }
});

objectsCanvas.addEventListener('mouseup', function (e) {
    if (isDragging) {
        isDragging = false;

        // Snap object to grid
        if (selectedObjectIndex >= 0) {
            const obj = objectsArray[selectedObjectIndex];
            const oldX = obj.x;
            const oldY = obj.y;

            // Round to nearest  meter
            obj.x = Math.round(obj.x);
            obj.y = Math.round(obj.y);

            // Check if snapped position causes overlap
            if (hasCollision(obj, selectedObjectIndex)) {
                // Revert to pre-snap position if overlap occurs
                obj.x = oldX;
                obj.y = oldY;
            }

            redrawObjectsCanvas();
        }

        // Send updated objects back to Shiny
        Shiny.setInputValue('objects_updated', {
            objects: objectsArray,
            timestamp: Date.now()
        });
    }
});

objectsCanvas.addEventListener('mouseleave', function (e) {
    if (isDragging) {
        isDragging = false;

        // Snap object to grid (1 meter increments)
        if (selectedObjectIndex >= 0) {
            const obj = objectsArray[selectedObjectIndex];
            const oldX = obj.x;
            const oldY = obj.y;

            // Round to nearest  meter
            obj.x = Math.round(obj.x);
            obj.y = Math.round(obj.y);

            // Check if snapped position causes overlap
            if (hasCollision(obj, selectedObjectIndex)) {
                // Revert to pre-snap position if overlap occurs
                obj.x = oldX;
                obj.y = oldY;
            }

            redrawObjectsCanvas();
        }

        Shiny.setInputValue('objects_updated', {
            objects: objectsArray,
            timestamp: Date.now()
        });
    }
});

// Initialize canvas when document is ready
if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', initObjectsCanvas);
} else {
    initObjectsCanvas();
}
