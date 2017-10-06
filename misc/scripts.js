// from https://github.com/thSoft/SvgConnectors
var MiscUtils;
(function (MiscUtils) {
    function toArray(nodeList) {
        return Array.prototype.slice.call(nodeList);
    }
    MiscUtils.toArray = toArray;
    function getCenter(rectangle) {
        return {
            x: getMedian(rectangle.left, rectangle.right),
            y: getMedian(rectangle.top, rectangle.bottom)
        };
    }
    MiscUtils.getCenter = getCenter;
    function getMedian(a, b) {
        return Math.min(a, b) + Math.abs(a - b) / 2;
    }
    MiscUtils.getMedian = getMedian;
    function getIntersection(inner, outer, rectangle) {
        var height = outer.y - inner.y;
        var width = outer.x - inner.x;
        if (inner.x < outer.x) {
            var distanceFromRight = rectangle.right - inner.x;
            var rightY = inner.y + height * distanceFromRight / width;
            if ((rectangle.top <= rightY) && (rightY <= rectangle.bottom)) {
                return {
                    x: rectangle.right,
                    y: rightY
                };
            }
        }
        if (outer.x < inner.x) {
            var distanceFromLeft = inner.x - rectangle.left;
            var leftY = inner.y - height * distanceFromLeft / width;
            if ((rectangle.top <= leftY) && (leftY <= rectangle.bottom)) {
                return {
                    x: rectangle.left,
                    y: leftY
                };
            }
        }
        if (inner.y < outer.y) {
            var distanceFromBottom = rectangle.bottom - inner.y;
            var bottomX = inner.x + width * distanceFromBottom / height;
            if ((rectangle.left <= bottomX) && (bottomX <= rectangle.right)) {
                return {
                    x: bottomX,
                    y: rectangle.bottom
                };
            }
        }
        if (outer.y < inner.y) {
            var distanceFromTop = inner.y - rectangle.top;
            var topX = inner.x - width * distanceFromTop / height;
            if ((rectangle.left <= topX) && (topX <= rectangle.right)) {
                return {
                    x: topX,
                    y: rectangle.top
                };
            }
        }
        return null;
    }
    MiscUtils.getIntersection = getIntersection;
})(MiscUtils || (MiscUtils = {}));
///<reference path="MiscUtils.ts"/>
var SvgConnectors;
(function (SvgConnectors) {
    function manage() {
        refreshAll();
        document.addEventListener("DOMSubtreeModified", refreshAll);
        window.addEventListener("resize", refreshAll);
    }
    SvgConnectors.manage = manage;
    function refreshAll() {
        var sourceAttribute = "data-source";
        var targetAttribute = "data-target";
        var connectors = MiscUtils.toArray(document.querySelectorAll("[" + sourceAttribute + "][" + targetAttribute + "]"));
        connectors.forEach(function (node) {
            var connector = node;
            var source = document.getElementById(connector.getAttribute(sourceAttribute));
            var target = document.getElementById(connector.getAttribute(targetAttribute));
            if (source != null && target != null) connect(connector, source, target);
        });
    }
    function connect(connectorNode, source, target) {
        refresh(connectorNode, source, target);
        observe(source, connectorNode, source, target);
        observe(target, connectorNode, source, target);
    }
    function refresh(connectorNode, source, target) {
        if (connectorNode instanceof SVGElement) {
            var connector = connectorNode;
            var sourceRect = toPage(source.getBoundingClientRect());
            var targetRect = toPage(target.getBoundingClientRect());
            var sourceCenter = MiscUtils.getCenter(sourceRect);
            var targetCenter = MiscUtils.getCenter(targetRect);
            var startPoint = MiscUtils.getIntersection(sourceCenter, targetCenter, sourceRect);
            var endPoint = MiscUtils.getIntersection(targetCenter, sourceCenter, targetRect);
            switch (connector.tagName.toLowerCase()) {
                case "line":
                    connector.setAttribute("x1", startPoint.x.toString());
                    connector.setAttribute("y1", startPoint.y.toString());
                    connector.setAttribute("x2", endPoint.x.toString());
                    connector.setAttribute("y2", endPoint.y.toString());
                    break;
                case "text":
                    connector.setAttribute("text-anchor", "middle");
                    var x = MiscUtils.getMedian(startPoint.x, endPoint.x);
                    connector.setAttribute("x", x.toString());
                    var y = MiscUtils.getMedian(startPoint.y, endPoint.y);
                    connector.setAttribute("y", y.toString());
                    break;
                case "g":
                    var children = MiscUtils.toArray(connector.childNodes);
                    children.forEach(function (child) { return refresh(child, source, target); });
                    break;
            }
            var containerSvg = connector.ownerSVGElement;
            if (containerSvg != null) {
                var scrollWidth = Math.max(document.documentElement.scrollWidth, document.body.scrollWidth); // XXX cross-browser behavior
                containerSvg.setAttribute("width", scrollWidth.toString());
                var scrollHeight = Math.max(document.documentElement.scrollHeight, document.body.scrollHeight); // XXX cross-browser behavior
                containerSvg.setAttribute("height", scrollHeight.toString());
            }
        }
    }
    function toPage(rectangle) {
        return {
            left: rectangle.left + window.pageXOffset,
            top: rectangle.top + window.pageYOffset,
            right: rectangle.right + window.pageXOffset,
            bottom: rectangle.bottom + window.pageYOffset,
            width: rectangle.width,
            height: rectangle.height
        };
    }
    function observe(observed, connectorNode, source, target) {

    }
})(SvgConnectors || (SvgConnectors = {}));
