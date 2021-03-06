async function fetchLayout(context) {
    //var response = await fetch("/strace/1?start=400%251&end=401%251");
    var response = await fetch("/strace/1");
    //var response = await fetch("/strace/1?start=1600%254&end=1602%254");
    if (!response.ok) {
        throw new Error("fetchLayout failed: " + response.statusText);
    }
    return JSON.parse(await response.text());
}

// IDEA: Make normal colour of rect 50% saturation and use 100% to indicate syscalls.

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

function toRatio(number) {
    return encodeURIComponent(`${Math.floor(number * 1000000)} % 1000000`);
}

function rgbHue(h) {
    const s = 0.6
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
        var rect = createSvgElement("rect", {
            x: `${x}%`,
            width: `${width}%`,
            y: `${(index + 0.1) * rowHeight}%`,
            height: `${0.8 * rowHeight}%`,
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
    var hue = 0;     // smaller spacer size bases have all processes under a root closer in colour
    const spacerSize = depth => Math.pow(0.1, depth);
    const totalSize = colourDistribution.filter(Number.isInteger).map(spacerSize).reduce((x, y) => x + y);
    colourDistribution.forEach(item => {
        if (Array.isArray(item)) {
            item.forEach(element => {
                switch (element.localName) {
                    case "rect":
                        element.setAttribute("fill", rgbHue(hue));
                        break;
                    case "line":  // Consider using: https://vanseodesign.com/web-design/svg-linear-gradients/
                        element.setAttribute("stroke", rgbHue(hue));
                        break;
                }
            });
        } else {
            hue += (360 / totalSize) * spacerSize(item);
        }
    });
    var graph = createSvgElement("g", { id: "graph" });
    elements.forEach(element => graph.appendChild(element));
    return graph;
}

// Consider changing some uses of Object to Map

function animate2(target, at) {
    const start = performance.now();
    const source = document.getElementById("graph").cloneNode(true);
    const getKey = dataset => JSON.stringify({
        pid: dataset.pid,
        start: dataset.start,
        end: dataset.end
    });
    const elements = { };
    for (let element of source.children) {
        elements[getKey(element.dataset)] = {
            source: element
        }
    }
    for (let element of target.children) {
        // will need to add when there wasn't a source
    }
    // TODO: Decouple temporal and process dimensions and animate them separately.
    // Start the temporal animation immediately before fetching the new layout.
    // Start the process dimension animation when the fetch returns and speed up temporal animation.

    // Map NodeKey (SourceElement, TargetElement)
    // Build the above map using the cloned source, provided target, and contextually computed
    // TODO: add any rect elements in target that are new
    return window.requestAnimationFrame(timestamp => {
        const t = (timestamp - start) / (at - start);
        for (let element of source.children) {
            const animateAttribute = name => {
                
            }
            //console.log(element);
        }
        //const getValue = key => 

    });
}

function rangeSelectionControl(svg, { minLabel, maxLabel, getLabelForPercentage, onChange }) {
    const debugOpacity = "0.3";
    const rescaleHandlers = [ ];
    const rescale = () => {
        const fullWidth = 4096;
        const fullHeight = 2048;
        const actualWidth = svg.getBoundingClientRect().width * window.devicePixelRatio;
        const scale = px => Math.ceil(px * 1920 / actualWidth);
        const padding = scale(12);
        const textHeight = scale(48);
        const controlHeight = 2 * (padding + textHeight);
        const controlTop = fullHeight - controlHeight;
        rescaleHandlers.forEach(f => f({
            fullWidth,
            fullHeight,
            textHeight,
            handleWidth: scale(32),
            padding,
            controlHeight,
            controlTop,
            controlMidline: controlTop + controlHeight / 2,
        }));
    };
    window.setTimeout(rescale, 0);
    window.addEventListener("resize", rescale);
    const control = createSvgElement("g", { });
    const responsive = (name, baseAttributes, calculateAttributes) => {
        const element = createSvgElement(name, baseAttributes);
        control.append(element);
        rescaleHandlers.push(scale => {
            const attributes = calculateAttributes(scale);
            for (const name in attributes) {
                element.setAttribute(name, attributes[name]);
            }
        });
        return element;
    };
    responsive("rect", {
        "fill": "#efefef",
        "stroke": "#7f7f7f",
    }, scale => ({
        "x": "0",
        "y": `${scale.controlTop}`,
        "width": `${scale.fullWidth}`,
        "height": `${scale.controlHeight}`,
    }));
    const text = (content, baseAttributes, calculateAttributes) => {
        const element = responsive("text", baseAttributes, calculateAttributes);
        element.textContent = content;
        return element;
    }
    const earliest = text(minLabel, {
        "text-anchor": "start",
        "dominant-baseline": "text-top",
        "fill": "#7f7f7f",
    }, scale => ({
        "font-size": `${scale.textHeight}px`,
        "x": `${scale.padding}`,
        "y": `${scale.controlMidline - scale.padding}`,
    }));
    const latest = text(maxLabel, {
        "text-anchor": "end",
        "dominant-baseline": "text-top",
        "fill": "#7f7f7f",
    }, scale => ({
        "font-size": `${scale.textHeight}px`,
        "x": `${scale.fullWidth - scale.padding}`,
        "y": `${scale.controlMidline - scale.padding}`,
    }));
    const start = text(minLabel, {
        "text-anchor": "end",
        "dominant-baseline": "hanging",
        "fill": "#2f2f2f",
    }, scale => ({
        "font-size": `${scale.textHeight}px`,
        "x": `${768}`,
        "y": `${scale.controlMidline + scale.padding}`,
    }));
    const end = text(maxLabel, {
        "text-anchor": "start",
        "dominant-baseline": "hanging",
        "fill": "#2f2f2f",
    }, scale => ({
        "font-size": `${scale.textHeight}px`,
        "x": `${2768}`,
        "y": `${scale.controlMidline + scale.padding}`,
    }));
    const selectionLeft = scale => start.getBBox().x + start.getBBox().width + scale.padding;
    const selectionWidth = scale => end.getBBox().x - scale.padding - selectionLeft(scale);
    responsive("rect", {
        "fill": "#a8def0",
        "stroke": "#7f7f7f",
    }, scale => ({
        "x": `${selectionLeft(scale)}`,
        "y": `${scale.controlMidline - scale.controlHeight / 2}`,
        "width": `${selectionWidth(scale)}`,
        "height": `${scale.controlHeight}`,
    }));
    const cursorInSvg = e => {
        var point = svg.createSVGPoint();
        point.x = e.clientX;
        point.y = e.clientY;
        return point.matrixTransform(svg.getScreenCTM().inverse());
    };
    var activeHandle = null;
    document.addEventListener("mousemove", e => {
        if (activeHandle) {
            const cursor = cursorInSvg(e);
            activeHandle.element.setAttribute("x", `${activeHandle.bbox.x + cursor.x - activeHandle.down.x}`);
            //console.log(`x = ${point.x}, y = ${point.y}`)
        }
    });
    const makeHandle = (element, getLeftBound, getRightBound) => {
        element.addEventListener("mousedown", e => {
            const point = cursorInSvg(e);
            activeHandle = { element,
                bbox: element.getBBox(),
                down: point,
            };
            console.log(activeHandle);
        });
        element.addEventListener("mouseup", e => {
            activeHandle = null;
        });
    };
    const startHitBox = responsive("rect", {
        "opacity": debugOpacity,
        "fill": "#ffff00",
        "stroke": "#ffff00",
        "style": "cursor: ew-resize",
    }, scale => ({
        "x": `${selectionLeft(scale) - scale.handleWidth / 2}`,
        "y": `${scale.controlMidline - scale.controlHeight / 2}`,
        "width": `${scale.handleWidth}`,
        "height": `${scale.controlHeight}`,
    }));
    const endHitBox = responsive("rect", {
        "opacity": debugOpacity,
        "fill": "#ffff00",
        "stroke": "#ffff00",
        "style": "cursor: ew-resize",
    }, scale => ({
        "x": `${selectionLeft(scale) + selectionWidth(scale) - scale.handleWidth / 2}`,
        "y": `${scale.controlMidline - scale.controlHeight / 2}`,
        "width": `${scale.handleWidth}`,
        "height": `${scale.controlHeight}`,
    }));
    makeHandle(startHitBox);
    makeHandle(endHitBox);
    // Consider using this for the handles: https://codepen.io/FelixRilling/pen/qzfoc
    // Might need: https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feDropShadow

    responsive("line", {
        "stroke": "#ff3f3f",
        "stroke-width": "2",
        "opacity": `${debugOpacity}`,
    }, scale => ({
        "x1": "0",
        "y1": `${scale.controlMidline}`,
        "x2": "4096",
        "y2": `${scale.controlMidline}`,
    }));
    responsive("line", {
        "stroke": "#ff3f3f",
        "stroke-width": "2",
        "opacity": `${debugOpacity}`,
    }, scale => ({
        "x1": "0",
        "y1": `${scale.controlMidline - scale.controlHeight / 4}`,
        "x2": "4096",
        "y2": `${scale.controlMidline - scale.controlHeight / 4}`,
    }));
    responsive("line", {
        "stroke": "#ff3f3f",
        "stroke-width": "2",
        "opacity": `${debugOpacity}`,
    }, scale => ({
        "x1": "0",
        "y1": `${scale.controlMidline + scale.controlHeight / 4}`,
        "x2": "4096",
        "y2": `${scale.controlMidline + scale.controlHeight / 4}`,
    }));
    return control;
}

function timeSelectionControl(svg, { min, max, onChange }) {
    const formatTime = seconds => {
        const part = n => `${Math.floor(n)}`.padStart(2, "0");
        const hours = part(Math.floor(seconds / 3600));
        const minutes = part(Math.floor((seconds - hours * 3600) / 60));
        return `${hours}:${minutes}:${part(seconds % 60)}`;
    };
    return rangeSelectionControl(svg, {
        minLabel: `${formatTime(min)}`,
        maxLabel: `${formatTime(max)}`,
        getLabelForPercentage: p => formatTime(p * 86400),
        onChange: onChange,
    });
}

function appendTimeControl(svg) {
    const handleSize = 16;
    const textHeight = 48;
    const debugOpacity = 0.2;
    const control = createSvgElement("g", { });
    svg.append(control);
//    const style = createSvgElement("style", { });
//    style.textContent = `
//
//    `;
//    control.append(style);
    const actualWidth = svg.getBoundingClientRect().width * window.devicePixelRatio;
    const fixedSize = px => Math.ceil(px * 1920 / actualWidth);
    const text = (x, y, anchor, baseline, content) => {
        const element = createSvgElement("text", { x, y,
            "text-anchor": anchor,
            "dominant-baseline": baseline,
            "font-size": `${fixedSize(textHeight)}px`,
            "fill": "#2f2f2f",
        });
        element.textContent = content;
        return element;
    }
    const baselineOffset = 12;
    const controlHeight = fixedSize(2 * (baselineOffset + textHeight));
    appendSvgElement(control, "rect", {
        x: "0",
        y: `${512 - controlHeight / 2}`,
        width: "4096",
        height: `${controlHeight}`,
        fill: "#efefef",
        stroke: "#7f7f7f",
    });
    control.append(appendSvgElement(svg, "line", {
        x1: "0",
        y1: "512",
        x2: "4096",
        y2: "512",
        stroke: "#3f3f3f",
        "stroke-width": "3",
        "stroke-dasharray": "16 4",
        opacity: debugOpacity,
    }));
    control.append(appendSvgElement(svg, "line", {
        x1: "0",
        y1: `${512 - controlHeight / 4}`,
        x2: "4096",
        y2: `${512 - controlHeight / 4}`,
        stroke: "#ff3f3f",
        "stroke-width": "3",
        opacity: debugOpacity,
    }));
    control.append(appendSvgElement(svg, "line", {
        x1: "0",
        y1: `${512 + controlHeight / 4}`,
        x2: "4096",
        y2: `${512 + controlHeight / 4}`,
        stroke: "#3fff3f",
        "stroke-width": "3",
        opacity: debugOpacity,
    }));
    const earliest = text(fixedSize(12), 512 - fixedSize(12), "start", "text-top", "00:06:32");
    control.append(earliest);
    const latest = text(4096 - fixedSize(12), 512 - fixedSize(12), "end", "text-top", "00:07:15");
    control.append(latest);
    const start = text(768, 512 + fixedSize(12), "end", "hanging", "00:06:35");
    control.append(start);
    const end = text(2768, 512 + fixedSize(12), "start", "hanging", "00:07:02");
    control.append(end);
    const selectionLeft = start.getBBox().x + start.getBBox().width + fixedSize(12);
    const selectionWidth = end.getBBox().x - fixedSize(12) - selectionLeft;
    appendSvgElement(control, "rect", {
        x: `${selectionLeft}`,
        y: `${512 - controlHeight / 2}`,
        width: `${selectionWidth}`,
        height: `${controlHeight}`,
        fill: "#a8def0",
        stroke: "#7f7f7f",
    });
    const cursorInSvg = e => {
        var point = svg.createSVGPoint();
        point.x = e.clientX;
        point.y = e.clientY;
        return point.matrixTransform(svg.getScreenCTM().inverse());
    };
    var activeHandle = null;
    document.addEventListener("mousemove", e => {
        if (activeHandle) {
            const cursor = cursorInSvg(e);
            activeHandle.element.setAttribute("x", `${activeHandle.bbox.x + cursor.x - activeHandle.down.x}`);
            //console.log(`x = ${point.x}, y = ${point.y}`)
        }
    });
    const makeHandle = (element, getLeftBound, getRightBound) => {
        element.addEventListener("mousedown", e => {
            const point = cursorInSvg(e);
            activeHandle = { element,
                bbox: element.getBBox(),
                down: point,
            };
            console.log(activeHandle);
        });
        element.addEventListener("mouseup", e => {
            activeHandle = null;
        });
    };
    const startHitBox = createSvgElement("rect", {
        x: `${selectionLeft - fixedSize(handleSize)}`,
        y: `${512 - controlHeight / 2}`,
        width: `${fixedSize(handleSize * 2)}`,
        height: `${controlHeight}`,
        opacity: debugOpacity,
        fill: "#ffff00",
        stroke: "#ffff00",
        style: "cursor: ew-resize",
    });
    control.append(startHitBox);
    const endHitBox = createSvgElement("rect", {
        x: `${selectionLeft + selectionWidth - fixedSize(handleSize)}`,
        y: `${512 - controlHeight / 2}`,
        width: `${fixedSize(handleSize * 2)}`,
        height: `${controlHeight}`,
        opacity: debugOpacity,
        fill: "#ffff00",
        stroke: "#ffff00",
        style: "cursor: w-resize",
    });
    control.append(endHitBox);
    makeHandle(startHitBox);
    makeHandle(endHitBox);
    // Consider using this for the handles: https://codepen.io/FelixRilling/pen/qzfoc
    // Might need: https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feDropShadow
}

// Should strace-viewer be named strace-explorer instead? The increasing interactivity might merit this.
// e.g. glow highlighting process subtrees and allowing them to be hidden

async function fetchFullContext() {
    
    var response = await fetch("/strace/1");
    //var response = await fetch("/strace/1?start=1600%254&end=1602%254");
    if (!response.ok) {
        throw new Error("fetchLayout failed: " + response.statusText);
    }
    return JSON.parse(await response.text());
}

async function dataSource() {
    async function fetchData(start, end) {
        var queryParams = [ ];
        if (start != null) {
            query.push(`start=${toRatio(start)}`);
        }
        if (end != null) {
            query.push(`end=${toRatio(end)}`);
        }
        var response = await fetch(`/strace/1?${queryParams.join("&")}`);
        if (!response.ok) {
            throw new Error("fetchData failed: " + response.statusText);
        }
        return JSON.parse(await response.text());
    }
    const foldMapNullableRatio = (layout, f, g) =>
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
    const minTime = layout => foldMapNullableRatio(layout, Math.min, row => row[0].key.start);
    const maxTime = layout => foldMapNullableRatio(layoyt, Math.max, row => row[row.length - 1].key.end);
    const fullContext = await fetchData();
    /* Things that need controlling:
        - animating the temporal dimension: always update on setTime for smooth lagless feel
        - requesting new layout: wait 1000ms before initiating a fetch and only have one fetch in progress at any moment
            Actions to take on fetch():
            - no fetch: setTimeout 1000ms and enter 'fetch pending' state (start fetch in callback)
            - fetch pending: update context of pending fetch but leave timer alone
            - fetch in progress: queue a pending fetch after this one finishes
     */

    const state = { active: false };
    const setPending = context => state.pending = context;
    const fetchPending = () => {
        fetchData(state.pending.start, state.pending.end).then(layout => {
            if (state.pending) {
                fetchPending();
            } else {
                state.active = false;
            }
        });
        state.pending = null;
    };

    // TBC: Render time selection and controls for adjusting it.

    const setTime = (start, end, immediate) => {
        setPending({ start, end });
        if (!state.active) {
            window.setTimeout(fetchPending, 1000);
            state.active = true;
        }
    };

    return {
        setStartTime: setTime
    }

/*
        setStartTime(time, immediate)
        setEndTime(time, immediate)
*/
}

function animate(duration, action) {
    const start = performance.now();
    const end = start + duration;
    const animation = {
        cancel: function() {
            window.cancelAnimationFrame(this.id);
        }
    };
    const nextFrame = () => window.requestAnimationFrame(now => {
        const t = (now - start) / (end - start);
        //console.log(`now = ${now}, start = ${start}, end = ${end}, t = ${t}`);
        action(Math.min(t, 1));
        if (t < 1) {
            animation.id = nextFrame();
        }
    });
    animation.id = nextFrame();
    return animation;
}

window.onload = function() {
    const data = dataSource();
    var svg = appendSvgElement(document.body, "svg", {
        version: "2",
        viewBox:  "0 0 4096 2048",
        //style: "background: black;"
    });
    fetchLayout().then(layout => {
        const graph = render(layout);
//        svg.appendChild(graph);
//        animate2(graph, performance.now() + 1000);

        svg.append(timeSelectionControl(svg, {
            min: fromRatio("3920 % 10"), // 00:06:32
            max: fromRatio("4350 % 10"), // 00:07:15
            onChange: () => { },
        }));

//        appendTimeControl(svg);
    });
//    window.setTimeout(function() {
//      window.location.reload();
//    }, 1000);
}

function projectLayout() {
    alert("test!");
}

// vim: ts=4 sts=4 sw=4
