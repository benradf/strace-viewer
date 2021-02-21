async function fetchLayout(context) {
    var response = await fetch("/strace/1?start=400%251&end=401%251");
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

function appendSvgElement(parent, name, attributes) {
    var element = createSvgElement(name, attributes);
    parent.appendChild(element);
    return element;
}

function fromRatio(ratio) {
    const [ numerator, denominator ] = ratio.split("%");
    return parseInt(numerator) / parseInt(denominator);
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
    return layout.flatMap((row, index) => row.map(node => {
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
            fill: randomColour()
        });
        rect.dataset.pid = pid;
        rect.dataset.start = start;
        rect.dataset.end = end;
        return rect;
    }));

//    console.log("startTimeToPercentage(400) = " + String(startTimeToPercentage("405 % 1")));
//    console.log("bounds.min = " + String(minBound));
//    console.log("bounds.max = " + String(maxBound));
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
//    }, 3000);
}

function projectLayout() {
    alert("test!");
}
