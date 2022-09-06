---
layout: post
title: "Building a Custom Checkbox Component with Reactjs"
description: ""
categories: [javascript]
tags: []
thumbnail: /files/2022-05-10-css-trick-building-custom-checkbox/firefox.png
---

- [The basic HTML and CSS]({{ page.url }}#1-the-basic-html-and-css)
  - [Prepare the structure]({{ page.url }}#prepare-the-structure)
  - [Styling the Checkbox]({{ page.url }}#styling-the-checkbox)
  - [Styling different states]({{ page.url }}#styling-different-states)
- [The Reactjs component]({{ page.url }}#2-the-reactjs-component)
- [3. Handle other Checkbox states]({{ page.url }}#3-handle-other-checkbox-states)

> Ok, the story is that, I'm really bad at css. I have never worked on building any frontend
> component and I was given a task to build the Custom Checkbox component with Reactjs from scratch.
> Here is how...

# 1. The basic HTML and CSS

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

Try the live example in the below iframe (or [direct link](/files/2022-05-10-css-trick-building-custom-checkbox/structure.html))
<iframe height="100px" width="100%" style="border: 1px solid; border-radius: 8px" src="/files/2022-05-10-css-trick-building-custom-checkbox/structure.html"></iframe>

<!-- more -->

## Styling the Checkbox

The fact is that you cannot style the checkbox element. Each browser decides its own appearance of the
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

Live example (or [direct link](/files/2022-05-10-css-trick-building-custom-checkbox/basic-styling.html))
<iframe height="100px" width="100%" style="border: 1px solid; border-radius: 8px" src="/files/2022-05-10-css-trick-building-custom-checkbox/basic-styling.html"></iframe>

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

Live example (or [direct link](/files/2022-05-10-css-trick-building-custom-checkbox/different-state.html))
<iframe height="100px" width="100%" style="border: 1px solid; border-radius: 8px" src="/files/2022-05-10-css-trick-building-custom-checkbox/different-state.html"></iframe>

# 2. The Reactjs component

Converting this to a Reactjs component is quite straight forward with
[styled-components](https://styled-components.com/).

Here is the props type for our custom Checkbox component

```tsx
type MyCheckboxProps = {
  checked?: boolean;
  text: string;
  onChange?: (e: React.FormEvent<HTMLInputElement>) => void;
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
const CheckboxText = styled.div``;

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
```

Putting them altogether in the main component

```tsx
function MyCheckbox({
  checked = false,
  text,
  onChange
}: MyCheckboxProps): JSX.Element {
  return (
    <Label>
      <CheckboxInput {...{checked, onChange}} />
      <CheckboxDisplay />
      <CheckboxText>{text}</CheckboxText>
    </Label>
  );
}
```

# 3. Handle other Checkbox states

In reality, there are more states that you may want to have for the Checkbox beside `checked` and
`unchecked`. The HTML checkbox provides 2 more different states to use with css `indeterminate` and
`invalid`. They require some small tweaks in Javascript to enable. In order to activate those
states, we need to access to the DOM node directly via
[React Refs](https://reactjs.org/docs/refs-and-the-dom.html).

In the above `MyCheckbox` component, add custom handler to update the ref properties

```tsx
// add more props for the Checkbox component
type MyCheckboxProps = {
  checked?: boolean;
  indeterminate?: boolean;
  invalid?: boolean;
  text: string;
};

function MyCheckbox({
  checked = false,
  indeterminate = false,
  invalid = false,
  text
}: MyCheckboxProps): JSX.Element {
  return (
    <Label>
      <CheckboxInput
        {...{checked}}
        ref={(instance: HTMLInputElement | null) => {
          if (!instance) return;
          // set indeterminate state
          instance.indeterminate = indeterminate;
          // set invalid state
          instance.setCustomValidity(invalid ? 'error' : '');
        }}
      />
      <CheckboxDisplay/>
      <CheckboxText>{text}</CheckboxText>
    </Label>
  );
}
```

And then styling in css is straight forward

```tsx
const CheckboxInput = styled.input.attrs((props: MyCheckboxProps) => ({
  type: 'checkbox',
  ...props,
}))`
  /* ... same as previous CheckboxInput component */

  /* ... style the new state */
  &:indeterminate + ${CheckboxDisplay} {
    background-color: #00b3ee;
  }

  &:invalid + ${CheckboxText} {
    color: #ff0e29;
  }
`;
```

# Phew

That's all. Improve it yourself!
