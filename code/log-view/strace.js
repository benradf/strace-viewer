window.onload = function() {
  var createSvgElement = function(name, attributes) {
    const ns = "http://www.w3.org/2000/svg";
    var element = document.createElementNS(ns, name);
    for (const [ name, value ] of Object.entries(attributes)) {
      element.setAttribute(name, value);
    }
    return element;
  }
  var appendSvgElement = function(parent, name, attributes) {
    var element = createSvgElement(name, attributes);
    parent.appendChild(element);
    return element;
  }
  var svg = appendSvgElement(document.body, "svg", {
    version: "1.1",
    viewBox:  "0 0 4096 2048"
  });
  var rect = appendSvgElement(svg, "rect", {
    x: "100",
    y: "100",
    width: "3896",
    height: "1848",
    fill: "#3bc460"
  });
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
//  window.setTimeout(function() {
//    window.location.reload();
//  }, 1000);
}

function projectLayout() {
  alert("test!");
}
