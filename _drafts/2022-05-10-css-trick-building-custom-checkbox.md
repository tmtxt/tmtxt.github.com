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

Wrapping `<label>` around allows clicking on any element inside to transfer the
event to the corresponding `<input>` element without any Javascript (implicit association).

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

```html
<label class="mylabel">
    <input class="myinput" type="checkbox" name="checkbox" />
    <div class="mydisplay"></div>
    <div class="mylabel">Checkbox label</div>
</label>
```

```css
.myinput + .mydisplay {
    width: 20px;
    height: 20px;
    border: 3px solid darkgray;
    border-radius: 6px;
    box-sizing: border-box;
}
```

![Custom Display](/files/2022-05-10-css-trick-building-custom-checkbox/custom-display-1.png)

# Styling different states

hover + checked

```css
.myinput:hover + .mydisplay {
    border-color: #00b3ee;
}

.myinput:checked + .mydisplay {
    background-color: #00b3ee;
    border: none;
}
```

indeterminate

---

temp

```html
<label class="mylabel">
  <input class="myinput" type="checkbox" name="checkbox" />
  <div class="mydisplay">
    <svg width="20" height="21" viewBox="0 0 20 21" fill="#000000" xmlns="http://www.w3.org/2000/svg">
<path d="M8.33333 14.1833L5 10.85L6.175 9.67501L8.33333 11.825L13.825 6.33334L15 7.51668L8.33333 14.1833Z" fill="inherit"/>
</svg>
  </div>
  <div class="mylabel">Checkbox label</div>
</label>
<label class="mylabel">
  <input class="myinput" type="checkbox" name="checkbox" />
  <div class="mydisplay"></div>
  <div class="mylabel">Checkbox label</div>
</label>
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
    padding-bottom: 3px;
}

.myinput {
    display: none;
}

.myinput + .mydisplay {
    width: 20px;
    height: 20px;
    border: 3px solid darkgray;
    border-radius: 6px;
    box-sizing: border-box;
}

.myinput + .mydisplay > svg {
  display: none;
  width: 100%;
    height: 100%;
}

.myinput:checked + .mydisplay > svg {
  display: inline;
}

.myinput:hover + .mydisplay {
    border-color: #00b3ee;
}

.myinput:checked + .mydisplay {
    background-color: #00b3ee;
    border: none;
}
```