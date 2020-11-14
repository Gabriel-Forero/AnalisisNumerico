const lineByLine = require('n-readlines');
const readline = require("readline");
const fs = require("fs");

function mainCycle(rl, vertices) {
    rl.question("$¬ ", function (input) {
        let args = input.split(" ");
        switch (args[0]) {
            case "group_by":
                vertices = groupBy(vertices, args[1]);
                break;
            case "pick":
                vertices = vertices[Number(args[1])];
                break;
            case "sort_by":
                vertices = sortBy(vertices, args[1]);
                break;
            case "export":
                exportR(vertices, args[1], args[2]);
                break;
            case "export_csv":
                exportCSV(vertices, args[1]);
                break;
            case "print":
                console.log(vertices);
                break;
            case "slice":
                vertices = sliceQuadrant(vertices);
                break;
            case "export_e":
                exportREspecial(vertices, args[1]);
                break;
        }
        mainCycle(rl, vertices);
    });
}

function menu() {
    const rl = readline.createInterface({
        input: process.stdin,
        output: process.stdout
    });

    rl.question("Ruta del archivo PLY? ", function (name) {
        let vertices = leerPly(name);
        vertices = cleanVertices(vertices);
        console.log(`Cargados ${vertices.length} vertices.`);
        mainCycle(rl, vertices);
    });

    rl.on("close", function () {
        console.log("\nBYE BYE !!!");
        process.exit(0);
    });
}

class Vertex {
    x;
    y;
    z;

    constructor(x, y, z) {
        this.x = x;
        this.y = y;
        this.z = z;
    }
}


/**
 *
 * @param {String} sFile
 * @returns {Array<Vertex>}
 */
function leerPly(sFile) {
    const liner = new lineByLine(sFile);
    let line;
    while (line = liner.next()) {
        if (line.toString().startsWith("element vertex")) {
            break;
        }
    }
    let vertices = Number(line.toString().split(" ")[2]);
    while (line = liner.next()) {
        if (line.toString().startsWith("end_header")) {
            break;
        }
    }
    let verticesArray = [];
    for (let i = 0; i < vertices; i++) {
        line = liner.next();
        let vertCord = line.toString().split(" ");
        verticesArray.push(new Vertex(Number(vertCord[0]), Number(vertCord[1]), Number(vertCord[2])));
    }
    liner.close();
    return verticesArray;
}

/**
 * @param {Vertex} vertex
 */
function formatCSV(vertex){
    return `${vertex.x};${vertex.y};${vertex.z}\n`;
}

/**
 *
 * @param {Array<Vertex>} vertices
 * @param {String} file
 */
function exportCSV(vertices, file) {
    let oStream = fs.createWriteStream(file);
    oStream.write("x;y;z\n")
    for (let vertex of vertices){
        oStream.write(formatCSV(vertex));
    }
    oStream.close();
}

/**
 *
 * @param {Array<Vertex>} vertices
 * @return {Array<Vertex>}
 */
function cleanVertices(vertices) {
    for (let i = 0; i < vertices.length; i++) {
        if (vertices[i].x === -0) {
            vertices[i].x = 0;
        }
        if (vertices[i].y === -0) {
            vertices[i].y = 0;
        }
        if (vertices[i].z === -0) {
            vertices[i].z = 0;
        }
    }
    return vertices;
}

/**
 *
 * @param {Array<Vertex>} vertices
 * @param {String} axis
 * @return {Array<Vertex>}
 */
function sortBy(vertices, axis) {
    return vertices.sort((a, b) => {
        if (a[axis] < b[axis]) {
            return -1;
        } else if (a[axis] === b[axis]) {
            return 0;
        } else {
            return 1;
        }
    });
}

/**
 *
 * @param {Array<Vertex>} vertices
 * @param {String} axis axis
 * @param {String} variable
 * @return {String}
 */
function rFormat(vertices, axis, variable) {
    let formatted = `${variable} <- c( `;
    for (let i = 0; i < vertices.length; i++) {
        if (i !== vertices.length - 1) {
            formatted = formatted.concat(`${vertices[i][axis]}, `);
        } else {
            formatted = formatted.concat(`${vertices[i][axis]} )\n`);
        }
    }
    return formatted;
}


/**
 *
 * @param {Array<Vertex>} vertices
 * @param {String} axis
 * @returns {Array<Array<Vertex>>}
 */
function groupBy(vertices, axis) {
    let sortedVertices = sortBy(vertices, axis);
    let groupedVertices = [];
    let localVertices = [sortedVertices[0]];
    let localValue = sortedVertices[0][axis];
    for (let i = 1; i < sortedVertices.length; i++) {
        let newValue = sortedVertices[i][axis];
        if (newValue !== localValue) {
            groupedVertices.push(localVertices);
            localVertices = [sortedVertices[i]];
        } else {
            localVertices.push(sortedVertices[i]);
        }
        localValue = newValue;
    }
    return groupedVertices
}

/**
 *
 * @param {Array<Vertex>} vertices
 * @param {String} file
 * @param {String} axis
 */
function exportR(vertices, file, axis) {
    let oStream = fs.createWriteStream(file);
    let x = rFormat(vertices, "x", "x");
    oStream.write(x);
    let y = rFormat(vertices, "y", "y");
    oStream.write(y);
    let z = rFormat(vertices, "z", "z");
    oStream.write(z);
    let grouped = groupBy(vertices, axis);
    for (let i = 0; i < grouped.length; i++) {
        let x_i = rFormat(grouped[i], "x", `x_${i}`);
        oStream.write(x_i);
        let y_i = rFormat(grouped[i], "y", `y_${i}`);
        oStream.write(y_i);
        let z_i = rFormat(grouped[i], "z", `z_${i}`);
        oStream.write(z_i);
    }
    oStream.close();
}




function exportREspecial(vertices, file) {
    let oStream = fs.createWriteStream(file);
    vertices = sliceQuadrant(vertices);
    vertices = groupBy(vertices, "z");
    for (let i = 0; i < vertices.length; i++) {
        let localGroup = vertices[i];
        localGroup = sortBy(localGroup, "x");
        let x_i = rFormat(localGroup, "x", `x_${i}`);
        oStream.write(x_i);
        let y_i = rFormat(localGroup, "y", `y_${i}`);
        oStream.write(y_i);
        let z_i = rFormat(localGroup, "z", `z_${i}`);
        oStream.write(z_i);
    }
    oStream.close();
}

function sliceQuadrant(vertices) {
    let left = [];
    for (let i = 0; i < vertices.length; i++) {
        if (vertices[i].x >= 0 && vertices[i].y >= 0) {
            left.push(vertices[i]);
        }
    }
    return left;
}

menu();