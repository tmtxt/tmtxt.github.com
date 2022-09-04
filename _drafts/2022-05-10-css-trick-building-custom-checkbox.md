---
layout: post
title: "Building a Custom Checkbox Component with Reactjs"
description: ""
categories: [javascript]
tags: []
thumbnail: /files/2022-05-10-css-trick-building-custom-checkbox/firefox.png
---

- [1. The CSS and HTML]({{ page.url }}#1-the-css-and-html)
  - [Prepare the structure]({{ page.url }}#prepare-the-structure)
  - [Styling the Checkbox]({{ page.url }}#styling-the-checkbox)
  - [Styling different states]({{ page.url }}#styling-different-states)
- [2. The Reactjs component]({{ page.url }}#2-the-reactjs-component)

> Ok, the story is that, I'm really bad at css. I have never worked on building any frontend
> component and I was given a task to build the Custom Checkbox component with Reactjs from scratch.
> Here is how...

# 1. The CSS and HTML

## Prepare the structure

Here is how you usually create a checkbox with pure html and css. To avoid any complicated event
handler, I will simply wrap the `<label>` tag around, which allows clicking on any element inside to
transfer the event to the corresponding `<input>` element without any Javascript needed.

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
  margin: 2px;
}
```

Try the live example in the below iframe
<iframe width="100%" style="border: 1px solid;" src="/files/2022-05-10-css-trick-building-custom-checkbox/structure.html"></iframe>

## Styling the Checkbox

The fact is, you cannot style the checkbox element. Each browser decides its own appearance of the
checkbox. The approach is to hide the checkbox completely and replace with another element just for
the display.

Coming back to the above html structure, I'm going to add an empty `<div>` (`<span>`) after the
`<input>` element as the placeholder for the display

```html
<label class="mylabel">
  <input class="myinput" type="checkbox" name="checkbox" />
  <div class="mydisplay"></div> <!-- this is the new element -->
  <div class="mylabel">Checkbox label</div>
</label>
```

And then I need to hide the real checkbox and add any style that I like to the newly added element.
It's just a normal element so you are not limited to just checkbox style.

```css
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
```

Live example
<iframe style="border: 1px solid;" src="/files/2022-05-10-css-trick-building-custom-checkbox/basic-styling.html"></iframe>

## Styling different states

Since we have wrapped the `<label>` tag outside, clicking on any element inside will transfer the
event to the corresponding checkbox, which will then change its state. We can use that state to
style the display element like this

```css
.myinput:hover + .mydisplay {
  border-color: #00b3ee;
}

.myinput:checked + .mydisplay {
  background-size: cover;
  background-image: url("/files/2022-05-10-css-trick-building-custom-checkbox/check.png");
}
```

Live example
<iframe style="border: 1px solid;" src="/files/2022-05-10-css-trick-building-custom-checkbox/different-state.html"></iframe>

# 2. The Reactjs component

Converting this to a Reactjs component is quite straight forward with
[styled-components](https://styled-components.com/).

Here is the props type for our custom Checkbox component

```tsx
type MyCheckboxProps = {
  checked: boolean;
  text: string;
};
```

Here is how to convert those html to React component using `styled-components`. Instead of using css
class selector, we can refer to the component directly using its identifier and `&`

```tsx
import styled from 'styled-components';
import checkIcon from '/files/2022-05-10-css-trick-building-custom-checkbox/check.png';

const Label = styled.label`
  display: flex;
  gap: 5px;
  align-items: center;
  margin: 2px;
`;

const CheckboxDisplay = styled.div``;

const CheckboxInput = styled.input.attrs((props: MyCheckboxProps) => ({
  type: 'checkbox',
  ...props,
}))`
  display: none;

  & + ${CheckboxDisplay} {
    width: 20px;
    height: 20px;
    border: 3px solid darkgray;
    border-radius: 6px;
    box-sizing: border-box;
  }

  &:hover + ${CheckboxDisplay} {
    border-color: #00b3ee;
  }

  &:checked + ${CheckboxDisplay} {
    background-size: cover;
    background-image: url('${checkIcon}');
  }
`;

const CheckboxText = styled.div``;
```

Putting them altogether in the main component

```tsx
function MyCheckbox({ checked = false, text }: MyCheckboxProps): JSX.Element {
  return (
    <Label>
      <CheckboxInput {...{checked}} />
      <CheckboxDisplay />
      <CheckboxText>{text}</CheckboxText>
    </Label>
  );
}
```
