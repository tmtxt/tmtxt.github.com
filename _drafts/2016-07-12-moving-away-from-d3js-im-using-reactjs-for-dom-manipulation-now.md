---
layout: post
title: "Moving away from D3.js, I'm using React.js for DOM Manipulation now"
description: ""
categories: [javascript]
tags: []
thumbnail:
---

# Why React in place of D3?

So, I'm migrating my web app to full client-side rendering using **React** recently. The reason is
that I mixed both server-side rendering and client-side render too much. At first, all the pages use
server  rendering (something like jinja2 template). However, as a consequence of the increase in
interaction  with user on the web app, I started to add more and more js code, which leads to the
logic duplication in both backend and frontend. I decided to move all the rendering to React.js and
that makes my life much easier dealing with all the DOM manipulation and two-way binding.

The only
thing I need to deal with is the diagram that I implemented using **D3.js** before. I have been
researching on the internet for a good solution and I was very closed to follow one of those
tutorials, which suggests hooking **D3.js** rendering in **React**'s `componentDidMount` event
(actually, most of the tutorials suggest that). Suddenly, one of my frontend friend recommended me
throwing away **D3.js** for all those tasks. He said that **React.js** is very good at DOM
manipulation stuff, why I have to mixed D3 inside that to lose all the flexibility of two-way
binding, virtual DOM update,... Yeah, that sounds logical and I decided to give it a try, threw away
all my old code and started in a fresh new React way. Of course, I didn't **D3.js** completely, I
still use it because of its supporting functions for calculating the diagram position and
coordination.

# Implement the Tree diagram in React.js

Okay, the first thing I need to do is to convert this old piece of code from **D3** to **React**.
The requirement is to draw a family tree like this.
In contrast to my imagination, rendering the tree diagram using **React** is an amazingly
effortless task.

![tree diagram](/files/2016-07-12-moving-away-from-d3js-im-using-reactjs-for-dom-manipulation-now/tree.png)

### D3 Code

- Render the nodes and links (not exactly, but similar to this).

```javascript
// Calculate the tree diagram, nodes list position using d3
...

// Render the nodes
var nodeGroups = rootGroup.selectAll("g.node")
  .data(nodesList, function(d) { return d.id || (d.id = ++id); });

var nodeEnter = nodeGroups.enter().append("svg:g")
      .attr("class", "node")
      .attr("transform", function(d) { return "translate(" + d.x + "," + d.y + ")"; });
nodeEnter.append("svg:circle")
  .attr("r", 10)
  .style("fill", function(d) { return d._children ? "lightsteelblue" : "#fff"; });
nodeEnter.append("svg:text")
    .text(function(d) { return d.info["fullName"]; })
    .attr("y", -19)
    .attr("dy", ".35em")
    .attr("text-anchor", "middle")
    .style("fill-opacity", 1);
nodeEnter.append("svg:image")
    .attr("xlink:href", function(d){
      return d.info.picture;
    })
    .attr("x", -20)
    .attr("y", -68)
    .attr("height", "40px")
    .attr("width", "40px");

// Render the links
var linksGroup = rootGroup.selectAll("path.link")
      .data(linksList, function(d) { return d.target.id; });
linksGroup.enter().insert("svg:path", "g")
    .attr("class", "link")
    .attr("d", diagonal);
```

I skip some parts in the code to
calculate the nodes list position for demonstration. You can check the `D3` tree documentation
[here](https://github.com/d3/d3-hierarchy/blob/master/README.md#tree) or refer to the full
implementation on my Github
- [Calculate Nodes List](https://github.com/tmtxt/clojure-pedigree/blob/c94a97b64eba1ad2cc5eb6d7071c6dc0542e6400/svc.web/js/tree/render.js#L55)
- [Render Nodes](https://github.com/tmtxt/clojure-pedigree/blob/c94a97b64eba1ad2cc5eb6d7071c6dc0542e6400/svc.web/js/tree/nodes.js#L32)
- [Render Links](https://github.com/tmtxt/clojure-pedigree/blob/c94a97b64eba1ad2cc5eb6d7071c6dc0542e6400/svc.web/js/tree/links.js#L16)

The `D3` code is much similar to jQuery style, which makes me feel difficult to imagine the DOM
structure of the graph. It also requires me to explicitly update the tree graph everytime the data
changes (the user click on the button to expand or collapse the tree).

### React Code

What we need to render that graph are the person nodes and the
links. Each person node will be render as a group (`<g>`) of other elements (the person image,
person name and the circle button to expand or collapse the branch). Let's take a look at the React
style code.

```javascript
class TreeView extends Component {

  render() {
    /* get the pedigree tree data */
    const { treeData, containerWidth, containerHeight } = this.props;

    /* still use d3.js to calculate the tree layout and position of nodes, links */
    const treeLayout = d3.layout.tree().size([containerWidth, containerHeight]);
    const nodesList = treeLayout.nodes(root).reverse();
    const linksList = treeLayout.links(nodesList);
    const diagonal = d3.svg.diagonal().projection((d) => [d.x, d.y]);

    /* render the nodes */
    const nodes = nodesList.map(node => {
      return (
        <g key={node.id} className="node"
           transform={`translate(${node.x}, ${node.y})`}>
          <circle r="10" style={{'fill': node._children ? 'lightsteelblue' : '#fff'}} />
          <text y="-19" dy=".35em" textAnchor="middle"
                style={{'fillOpacity': 1}}>{node.fullName}</text>
          <image href={node.picture} x="-20" y="-68"
                 width="40px" height="40px"></image>
        </g>
      );
    });

    /* render the links */
    const links = linksList.map(link => {
      return (
        <path key={`${link.source.id}-${link.target.id}`} className="link"
              d={diagonal(link)} />
      );
    });

    return (
      <div className="tree-container">
        <svg height="1000" width={containerWidth}>
          <g>
            {links}
            {nodes}
          </g>
        </svg>
      </div>
    );
  }

}
```

As you can see from the code above, we still utilize `D3` but only to calculate the tree diagram,
the position of all nodes and links (yeah, why need to reinvent the wheel, just use what's already
there). It's much better and easier to understand, especially the svg tag like `<g>`, `<circle>`,
`<path>`,... We can also benefit from React two-way binding and DOM manipulation. There is no need
to explicitly call the render or update function. React does that automagically with it's excellent
virtual DOM.

# Responsive Diagram

The best thing when using React is it's two-way binding and auto DOM update when data changes.
Thanks to [react-dimensions](https://github.com/digidem/react-dimensions), I can easily make my
graph become a responsive one with just one line changed in my code. All I need to do is to wrap my
component inside the `Dimension` higher-order component

```javascript
const Dimensions = require('react-dimensions');

//
...

module.exports = Dimensions()(TreeView);
```

Code: [https://github.com/tmtxt/clojure-pedigree/blob/8e5360bc3f7707cf77d78c322f6d6db3d9cd58a9/dev.frontend/js/pages/tree_view/tree_view.jsx#L45](https://github.com/tmtxt/clojure-pedigree/blob/8e5360bc3f7707cf77d78c322f6d6db3d9cd58a9/dev.frontend/js/pages/tree_view/tree_view.jsx#L45)

# Graph Animation

Animation in React is a bit tricky. However, with the help of
[react-motion](https://github.com/chenglou/react-motion) and it's `<TransitionMotion />`, we can
easily simulate d3's `enter` and `exit` event. Simply implement the `willEnter` and `willLeave`
function of the `TransitionMotion`, you can achieve nearly the same result as can do in d3.


# Full demo code

The full code can be found on my Github, under the `clojure-pedigree` repo. It uses
[Baobab](https://github.com/Yomguithereal/baobab) to store data and
[baobab-react](https://github.com/Yomguithereal/baobab-react) for interacting with React

- [Main Tree View with react-dimensions](https://github.com/tmtxt/clojure-pedigree/blob/8e5360bc3f7707cf77d78c322f6d6db3d9cd58a9/dev.frontend/js/pages/tree_view/tree_view.jsx)
- [Nodes Rendering and Animation](https://github.com/tmtxt/clojure-pedigree/blob/8e5360bc3f7707cf77d78c322f6d6db3d9cd58a9/dev.frontend/js/pages/tree_view/nodes_group.jsx)
- [Links Rendering and Animation](https://github.com/tmtxt/clojure-pedigree/blob/8e5360bc3f7707cf77d78c322f6d6db3d9cd58a9/dev.frontend/js/pages/tree_view/links_group.jsx)

# Demo

![tree diagram](/files/2016-07-12-moving-away-from-d3js-im-using-reactjs-for-dom-manipulation-now/tree.gif)
