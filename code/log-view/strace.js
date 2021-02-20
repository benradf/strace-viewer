async function fetchLayout(context) {
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
    var foldMapNullableRatio = (f, g) =>
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
    var bounds = {
        min: foldMapNullableRatio(Math.min, row => row[0].key.start),
        max: foldMapNullableRatio(Math.max, row => row[row.length - 1].key.end)
    };
    console.log("bounds.min = " + String(bounds.min));
    console.log("bounds.max = " + String(bounds.max));
    var renderNode = function(node) {
        var rect = createSvgElement("rect", {
            
        });
        rect.dataset.pid = node.key.pid;
        rect.dataset.start = node.key.start;
        rect.dataset.end = node.key.end;
    }
}

window.onload = function() {
    var svg = appendSvgElement(document.body, "svg", {
        version: "2",
        viewBox:  "0 0 4096 2048"
    });
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
    fetchLayout().then(render);
//    window.setTimeout(function() {
//      window.location.reload();
//    }, 3000);
}

function projectLayout() {
    alert("test!");
}
