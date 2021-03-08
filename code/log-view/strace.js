const fullWidth = 4096;
const fullHeight = 2048;

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
    return parent.appendChild(element);
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

function render(data, bbox) {
    const foldMapNullableRatio = (f, g) =>
        data.layout.reduce((a, row) => {
            const x = g(row);
            if (x == null) {
                return a;
            } else if (a == null) {
                return fromRatio(x);
            } else {
                return f(a, fromRatio(x));
            }
        }, null);
//    const minBound = foldMapNullableRatio(Math.min, row => row[0].key.start);
//    const maxBound = foldMapNullableRatio(Math.max, row => row[row.length - 1].key.end);
    // TODO: Handle either or both bounds being null.
    const rowHeight = bbox.height / data.layout.length;
    const timeToPercentage = time => (fromRatio(time) - data.start) / (data.end - data.start);
    const pendingEdges = { };
    const colourDistribution = [ ];
    const elements = data.layout.flatMap((row, index) => row.flatMap(node => {
        const { pid, start, end } = node.key;
        const x = start == null ? 0.0 : timeToPercentage(start);
        const width = (end == null ? 1.0 : timeToPercentage(end)) - x;
        var rect = createSvgElement("rect", {
            x: `${bbox.x + x * bbox.width}`,
            width: `${width * bbox.width}`,
            y: `${bbox.y + (index + 0.1) * rowHeight}`,
            height: `${0.8 * rowHeight}`,
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
                "stroke-width": "1",
            }));
        }
        node.edges.forEach(edge => {
            const key = JSON.stringify(edge);
            if (pendingEdges[key] !== undefined) {
                //throw new Error("child process has multiple parents");
                // TODO: Fix this instead of ignoring it!
            }
            pendingEdges[key] = {
                y1: `${bbox.y + (index + 0.9) * rowHeight}`,
                depth: depth + 1
            };
        });
        colourDistribution.push(depth);
        colourDistribution.push(elements);
        return elements;
    }));
    if (colourDistribution.length > 0) {
        var hue = 0;     // smaller spacer size bases have all processes under a root closer in colour
        const spacerSize = depth => Math.pow(0.1, depth);
        const totalSize = colourDistribution.filter(Number.isInteger).map(spacerSize).reduce((x, y) => x + y);
        colourDistribution.forEach(item => {
            if (Array.isArray(item)) {
                item.forEach(element => {
                    switch (element.localName) {
                        case "rect":
                            element.setAttribute("fill", rgbHue(hue));
                            element.setAttribute("stroke", rgbHue(hue));
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
    }
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
    //const debugOpacity = 0.2;
    const debugOpacity = 0.0;
    const rescaleHandlers = [ ];
    const rescale = () => {
        const actualWidth = svg.getBoundingClientRect().width * window.devicePixelRatio;
        const scale = px => Math.ceil(px * 1920 / actualWidth);
        const padding = scale(12);
        const textHeight = scale(36);
        const controlHeight = 2 * (textHeight + padding);
        const controlTop = fullHeight - controlHeight;
        rescaleHandlers.forEach(f => f({
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
    responsive("line", {
        "opacity": "0.0",
    }, scale => ({
        "x1": `${0}`,
        "x2": `${fullWidth}`,
        "y1": `${scale.controlTop - scale.padding * 4}`,
        "y2": `${scale.controlTop - scale.padding * 4}`,
    }));
    responsive("rect", {
        "fill": "#efefef",
        "stroke": "#7f7f7f",
    }, scale => ({
        "x": `${0}`,
        "y": `${scale.controlTop}`,
        "width": `${fullWidth}`,
        "height": `${scale.controlHeight}`,
    }));
    const text = (content, title, baseAttributes, calculateAttributes) => {
        baseAttributes.style = `
            cursor: inherit;
        `;
        const element = responsive("text", baseAttributes, calculateAttributes);
        element.appendChild(createSvgElement("title", { })).append(title);
        element.append(content);
        return element;
    }
    const earliest = text(minLabel, "earliest", {
        "text-anchor": "start",
        "dominant-baseline": "text-top",
        "fill": "#7f7f7f",
    }, scale => ({
        "font-size": `${scale.textHeight}px`,
        "x": `${scale.padding}`,
        "y": `${scale.controlMidline - scale.padding / 2}`,
    }));
    const latest = text(maxLabel, "latest", {
        "text-anchor": "end",
        "dominant-baseline": "text-top",
        "fill": "#7f7f7f",
    }, scale => ({
        "font-size": `${scale.textHeight}px`,
        "x": `${fullWidth - scale.padding}`,
        "y": `${scale.controlMidline - scale.padding / 2}`,
    }));
    const rightOf = (element, scale) => element.getBBox().x + element.getBBox().width;
    const selectableRange = responsive("rect", {
        "opacity": `${0.5 * debugOpacity}`,
        "fill": "#7f7f7f",
        "stroke": "#7f7f7f",
    }, scale => ({
        "x": `${rightOf(earliest, scale) + scale.padding}`,
        "y": `${scale.controlMidline - scale.controlHeight / 2}`,
        "width": `${latest.getBBox().x - rightOf(earliest, scale) - scale.padding * 2}`,
        "height": `${scale.controlHeight}`,
    }));
    const cursorInSvg = e => {
        var point = svg.createSVGPoint();
        point.x = e.clientX;
        point.y = e.clientY;
        return point.matrixTransform(svg.getScreenCTM().inverse());
    };
    var activeHandle = null;
    document.addEventListener("mouseup", e => {
        document.body.style.cursor = "default";
        activeHandle = null;
    });
    document.addEventListener("mousemove", e => {
        if (activeHandle) {
            const { x } = cursorInSvg(e);
            const percentage = activeHandle.onPercentageChange(Math.min(1, Math.max(0,
                activeHandle.downValue + (x - activeHandle.downX) / selectableRange.getBBox().width
            )));
            activeHandle.label.textContent = `${getLabelForPercentage(percentage)}`;
            document.body.style.cursor = activeHandle.hitbox.style.cursor;
            window.setTimeout(rescale, 0);
        }
    });
    const makeHandle = ({ label, hitbox, getValue, onPercentageChange }) => {
        hitbox.addEventListener("mousedown", e => {
            const { x } = cursorInSvg(e);
            activeHandle = {
                label,
                hitbox,
                downX: x,
                downValue: getValue(),
                onPercentageChange,
            };
        });
    };
    const getX = percentage => selectableRange.getBBox().x + percentage * selectableRange.getBBox().width;
    var start = 0.0, end = 1.0;
    responsive("rect", {
        //"fill": "#a8def0",
        "fill": "#cfcfcf",
        "stroke": "#7f7f7f",
    }, scale => ({
        "x": `${getX(start)}`,
        "y": `${scale.controlMidline - scale.controlHeight / 2}`,
        "width": `${Math.max(getX(end) - getX(start), 4)}`,
        "height": `${scale.controlHeight}`,
    }));
    makeHandle({
        getValue: () => start,
        onPercentageChange: percentage => {
            onChange(start = Math.min(percentage, end), end);
            return start;
        },
        label: text(minLabel, "start", {
            "text-anchor": "end",
            "dominant-baseline": "hanging",
            "fill": "#2f2f2f",
        }, scale => ({
            "font-size": `${scale.textHeight}px`,
            "x": `${getX(start) - scale.padding}`,
            "y": `${scale.controlMidline + scale.padding}`,
        })),
        hitbox: responsive("rect", {
            "opacity": `${0.1 * debugOpacity}`,
            "fill": "#ff0000",
            "stroke": "#ff0000",
        }, scale => ({
            "x": `${0}`,
            "y": `${scale.controlMidline - scale.controlHeight / 2}`,
            "width": `${getX(start) + (getX(end) - getX(start)) / 2}`,
            "height": `${scale.controlHeight}`,
            "style": `cursor: ${
                start == 0.0 ? "e-resize" :
                start == end ? "w-resize" :
                               "ew-resize"
            }`,
        })),
    });
    makeHandle({
        getValue: () => end,
        onPercentageChange: percentage => {
            onChange(start, end = Math.max(start, percentage));
            return end;
        },
        label: text(maxLabel, "end", {
            "text-anchor": "start",
            "dominant-baseline": "hanging",
            "fill": "#2f2f2f",
        }, scale => ({
            "font-size": `${scale.textHeight}px`,
            "x": `${getX(end) + scale.padding}`,
            "y": `${scale.controlMidline + scale.padding}`,
        })),
        hitbox: responsive("rect", {
            "opacity": `${0.1 * debugOpacity}`,
            "fill": "#ffff00",
            "stroke": "#ffff00",
        }, scale => ({
            "x": `${getX(end) - (getX(end) - getX(start)) / 2}`,
            "y": `${scale.controlMidline - scale.controlHeight / 2}`,
            "width": `${fullWidth - getX(end) + (getX(end) - getX(start)) / 2}`,
            "height": `${scale.controlHeight}`,
            "style": `cursor: ${
                end == start ? "e-resize" :
                end == 1.0   ? "w-resize" :
                               "ew-resize"
            }`,
        })),
    });
    //
    // TBC: Use start and end elements as canonical selection range
    // On mousemove event:
    //  1. move the active element (start or end)
    //  2. rescale the rest of the timer control
    //  3. raise onChange event with start and end derived percentage
    //
    // Consider using this for the handles: https://codepen.io/FelixRilling/pen/qzfoc
    // Might need: https://developer.mozilla.org/en-US/docs/Web/SVG/Element/feDropShadow
    responsive("line", {
        "stroke": "#3f3f3f",
        "stroke-width": "2",
        "opacity": `${1.0 * debugOpacity}`,
    }, scale => ({
        "x1": "0",
        "y1": `${scale.controlMidline}`,
        "x2": `${fullWidth}`,
        "y2": `${scale.controlMidline}`,
    }));
    responsive("line", {
        "stroke": "#3f3f3f",
        "stroke-width": "2",
        "opacity": `${1.0 * debugOpacity}`,
    }, scale => ({
        "x1": "0",
        "y1": `${scale.controlMidline - scale.controlHeight / 4}`,
        "x2": `${fullWidth}`,
        "y2": `${scale.controlMidline - scale.controlHeight / 4}`,
    }));
    responsive("line", {
        "stroke": "#3f3f3f",
        "stroke-width": "2",
        "opacity": `${1.0 * debugOpacity}`,
    }, scale => ({
        "x1": "0",
        "y1": `${scale.controlMidline + scale.controlHeight / 4}`,
        "x2": `${fullWidth}`,
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
        getLabelForPercentage: p => formatTime(min + p * (max - min)),
        onChange: (start, end) => onChange(min + start * (max - min), min + end * (max - min)),
    });
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

async function fetchLoop(onLayoutChange) {
    async function fetchData(start, end) {
        var queryParams = [ ];
        if (start != null) {
            queryParams.push(`start=${toRatio(start)}`);
        }
        if (end != null) {
            queryParams.push(`end=${toRatio(end)}`);
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
    const fullContext = await fetchData();
    const minTime = foldMapNullableRatio(fullContext, Math.min, row => row[0].key.start);
    const maxTime = foldMapNullableRatio(fullContext, Math.max, row => row[row.length - 1].key.end);
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
        const start = state.pending.start;
        const end = state.pending.end;
        fetchData(start, end).then(layout => {
            onLayoutChange({ layout, start, end });
            if (state.pending) {
                fetchPending();
            } else {
                state.active = false;
            }
        });
        state.pending = null;
    };

    // TBC: Render time selection and controls for adjusting it.

//    const setTime = (start, end, immediate) => {
//        setPending({ start, end });
//        if (!state.active) {
//            window.setTimeout(fetchPending, 1000);
//            state.active = true;
//        }
//    };

    return {
        minTime,
        maxTime,
        setTimeSelection: (start, end, immediate) => {
            console.log(`start = ${start}, end = ${end}`);
            setPending({ start, end });
            if (!state.active) {
                window.setTimeout(fetchPending, immediate? 0 : 100);
                state.active = true;
            }
        },
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
    document.body.style.cursor = "default";
    const svg = appendSvgElement(document.body, "svg", {
        version: "2",
        viewBox:  `0 0 ${fullWidth} ${fullHeight}`,
    });
    var graph = null;
    var timeControl = null;
    fetchLoop(data => {
        if (graph) {
            graph.parentNode.removeChild(graph);
        }
        graph = render(data, {
            x: 0,
            y: 0,
            width: fullWidth,
            height: fullHeight - timeControl.getBBox().height,
        });
        svg.appendChild(graph);
    }).then(source => {
        source.setTimeSelection(source.minTime, source.maxTime, true);
        svg.append(timeControl = timeSelectionControl(svg, {
            min: source.minTime,
            max: source.maxTime,
            onChange: (start, end) => source.setTimeSelection(start, end),
        }));
    });
//    fetchLayout().then(layout => {
////        animate2(graph, performance.now() + 1000);
//
//
////        appendTimeControl(svg);
//    });
//    window.setTimeout(function() {
//      window.location.reload();
//    }, 1000);
}

function projectLayout() {
    alert("test!");
}

// vim: ts=4 sts=4 sw=4
