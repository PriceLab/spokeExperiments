vizmap = [
   {"selector":"node", "css": {
       "text-valign":"center",
       "text-halign":"center",
       "shape": "ellipse",
       "border-color": "black",
       "background-color": "beige",
       "content": "data(id)",
       "font-size": "28px",
       "border-width": "1px",
       "width": "mapData(degree, 0, 20, 100, 300)",
       "height": "mapData(degree, 0, 20, 100, 300)"
       }},

    {"selector": "node:selected", "css": {
       "overlay-opacity": 0.3,
       "overlay-color": "gray"
    }},

    {"selector": "edge", "css": {
        "curve-style": "bezier",
	"font-size": 32,
	"label": "data(distance)"
    }},

    {"selector": "edge:selected", "css": {
       "overlay-opacity": 0.3,
       "overlay-color": "gray"
    }},

]
