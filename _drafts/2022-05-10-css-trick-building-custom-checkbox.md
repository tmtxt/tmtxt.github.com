---
layout: post
title: "CSS Trick - Building Custom Checkbox"
description: ""
categories: []
tags: []
thumbnail: 
---

# The structure

```html
<label class="mylabel">
  <input class="myinput" type="checkbox" name="checkbox" />
  <div class="mydisplay"></div>
  <div class="mylabel">Checkbox label</div>
</label>
```
```css
.mylabel {
    display: flex;
    gap: 5px;
    align-items: center;
}
```

Wrapping `<label>` around allows clicking on any element inside to transfer to corresponding
event to the `<input>` element without any Javascript (implicit association).

- [The Input Label element](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/label)

Here are how it looks like on different browsers

![Firefox](/files/2022-05-10-css-trick-building-custom-checkbox/firefox.png)

![Chrome](/files/2022-05-10-css-trick-building-custom-checkbox/chrome.png)

# Styling the Checkbox

```css
.myinput {
    display: none;
}
```

Add custom display

- add an empty `<div>` (`<span>`) after the `<input>` element as the placeholder for the display

```css
.myinput + .mydisplay {
    width: 20px;
    height: 20px;
    border: 3px solid darkgray;
    border-radius: 6px;
    box-sizing: border-box;
}
```

- [Pseudo-elements](https://developer.mozilla.org/en-US/docs/Web/CSS/Pseudo-elements)

![Custom Display](/files/2022-05-10-css-trick-building-custom-checkbox/custom-display-1.png)

# Styling different states

hover

```css
.myinput:hover + .mydisplay {
    border-color: #00b3ee;
}

.myinput:checked + .mydisplay {
    background-color: #00b3ee;
    border: none;
}
```