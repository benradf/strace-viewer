async function fetchLayout(context) {
    //var response = await fetch("/strace/1?start=400%251&end=401%251");
    var response = await fetch("/strace/1");
    if (!response.ok) {
        throw new Error("fetchLayout failed: " + response.statusText);
    }
    return JSON.parse(await response.text());
}

function createSvgElement(name, attributes) {
    const ns = "http://www.w3.org/2000/svg";
    var element = document.createElementNS(ns, name);
    for (const [ name, value ] of Object.entries(attributes)) {
        element.setAttribute(name, value);
    }
    return element;
}

// TODO: Pass through the query string to the app as context to fetchLayout request. This allows bookmarks and history.

function appendSvgElement(parent, name, attributes) {
    var element = createSvgElement(name, attributes);
    parent.appendChild(element);
    return element;
}

function fromRatio(ratio) {
    const [ numerator, denominator ] = ratio.split("%");
    return parseInt(numerator) / parseInt(denominator);
}

function rgbHue(h) {
    const s = 1
    const l = 0.5
    const a = s * Math.min(l, 1 - l);
    const fracMod = (x, n) => x - Math.floor(Math.floor(x) / n) * n;
    const k = n => fracMod(n + h / 30, 12);
    const f = n => l - a * Math.max(-1, Math.min(Math.min(k(n) - 3, 9 - k(n)), 1));
    const scale = x => Math.floor(255 * x);
    return `rgb(${scale(f(0))},${scale(f(8))},${scale(f(4))})`;
}

function render(layout) {
    const foldMapNullableRatio = (f, g) =>
        layout.reduce((a, row) => {
            const x = g(row);
            if (x == null) {
                return a;
            } else if (a == null) {
                return fromRatio(x);
            } else {
                return f(a, fromRatio(x));
            }
        }, null);
    const minBound = foldMapNullableRatio(Math.min, row => row[0].key.start);
    const maxBound = foldMapNullableRatio(Math.max, row => row[row.length - 1].key.end);
    // TODO: Handle either or both bounds being null.
    const rowHeight = 100 / layout.length;
    const timeToPercentage = time => 100 * (fromRatio(time) - minBound) / (maxBound - minBound);
    const pendingEdges = { };
    const colourDistribution = [ ];
    const elements = layout.flatMap((row, index) => row.flatMap(node => {
        const { pid, start, end } = node.key;
        const x = start == null ? 0 : timeToPercentage(start);
        const width = (end == null ? 100 : timeToPercentage(end)) - x;
        const randomComponent = () => Math.floor(255 * Math.random());
        const randomColour = () => `rgb(${randomComponent()}, ${randomComponent()}, ${randomComponent()})`;
        var rect = createSvgElement("rect", {
            x: `${x}%`,
            width: `${width}%`,
            y: `${(index + 0.1) * rowHeight}%`,
            height: `${0.8 * rowHeight}%`,
            //fill: randomColour()
        });
        rect.dataset.pid = pid;
        rect.dataset.start = start;
        rect.dataset.end = end;
        const elements = [ rect ];
        const edge = pendingEdges[JSON.stringify(node.key)];
        const depth = edge !== undefined ? edge.depth : 0;
        if (edge !== undefined) {
            elements.push(createSvgElement("line", {
                x1: rect.getAttribute("x"),
                y1: edge.y1,
                x2: rect.getAttribute("x"),
                y2: rect.getAttribute("y"),
                //stroke: rect.getAttribute("fill")
            }));
        }
        node.edges.forEach(edge => {
            const key = JSON.stringify(edge);
            if (pendingEdges[key] !== undefined) {
                throw new Error("child process has multiple parents");
            }
            pendingEdges[key] = {
                y1: `${(index + 0.9) * rowHeight}%`,
                depth: depth + 1
            };
        });
        colourDistribution.push(depth);
        colourDistribution.push(elements);
        return elements;
    }));
    var hue = 0;
    const spacerSize = depth => Math.pow(0.5, depth);
    const totalSize = colourDistribution.filter(Number.isInteger).map(spacerSize).reduce((x, y) => x + y);
    colourDistribution.forEach(item => {
        if (Array.isArray(item)) {
            item.forEach(element => {
                switch (element.localName) {
                    case "rect":
                        element.setAttribute("fill", rgbHue(hue));
                        break;
                    case "line":
                        element.setAttribute("stroke", rgbHue(hue));
                        break;
                }
            });
        } else {
            hue += (360 / totalSize) * spacerSize(item);
        }
    });
    return elements;
}

window.onload = function() {
    var svg = appendSvgElement(document.body, "svg", {
        version: "2",
        viewBox:  "0 0 4096 2048"
    });
    fetchLayout().then(layout => render(layout).forEach(rect => svg.appendChild(rect)));
/*
    var rect = appendSvgElement(svg, "rect", {
        x: "100",
        y: "100",
        width: "3896",
        height: "1848",
        fill: "#3bc460"
    });
    rect.dataset.foo = "bar"
    var rect = appendSvgElement(svg, "rect", {
        x: "0",
        y: "2000",
        width: "4096",
        height: "48",
        fill: "#ebeff5"
    });
    var rect = appendSvgElement(svg, "rect", {
        x: "2200",
        y: "2000",
        width: "912",
        height: "48",
        fill: "#2ec7ff"
    });
*/
//    window.setTimeout(function() {
//      window.location.reload();
//    }, 15000);
}

function projectLayout() {
    alert("test!");
}
