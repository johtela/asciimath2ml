/**
 * # Editor for Testing AsciiMath Equations
 * 
 * This simple [web component][] allows typing AsciiMath equations and previews
 * the generated MathML elements.
 * 
 * We inherit the component from base class provided by [LiTScript][]. It 
 * attaches the stylesheet we specify to the component.
 * 
 * [web component]: https://developer.mozilla.org/en-US/docs/Web/API/Web_Components
 * [LiTScript]: https://johtela.github.io/litscript/
 */
import * as ce from 'litscript/lib/src/custom-elem'
import { asciiToMathML } from '.'
import "./asciimath-editor.css"
/**
 * ## Component Structure
 * 
 * The structure of our component is defined in the `label` and `content`
 * properties. Label is separate, because we must be able to change it's 
 * contents later or.
 */
export class AsciiMathEditor extends ce.StyledElement {
    private label = /*html*/`
            <span class="label">Preview</span>
    `
    private content = /*html*/`
        <textarea name="input" id="input" rows="5" spellcheck="false"
            placeholder="Enter AsciiMath Equation">
        </textarea>
        <div id="preview">
            ${this.label}
        </div>
    `
    /**
     * ## Constructor
     * 
     * We call the base constructor with the style sheet name we want to load.
     * The `.css` extension is omitted.
     */
    constructor() {
        super("asciimath-editor")
    }
    /**
     * ## Getters
     * 
     * Get the input element inside the component.
     */
    private get input(): HTMLTextAreaElement {
        return this.shadowRoot!.getElementById("input") as HTMLTextAreaElement
    }
    /**
     * Get the preview div inside the component.
     */
    private get preview(): HTMLDivElement {
        return this.shadowRoot!.getElementById("preview") as HTMLDivElement
    }
    /**
     * ## Update
     * 
     * When the contents of the input element changes we update the preview in
     * the method below. If there is no value for the input, we use the 
     * default preview contents. If we have a value, we: 
     * 
     * 1. convert it to MathML by calling `asciiToMathML` function,
     * 2. set it as the inner content of the preview div,
     * 3. format the contents of the div by calling the `format` method, and
     * 4. add a `<details>` tag that shows the MathML verbatim inside the 
     *    preview div.
     */
    private update() {
        let value = this.input.value
        let preview = this.preview
        if (!value)
            value = this.label
        else {
            value = asciiToMathML(value)
            preview.innerHTML = value
            this.format(preview, 0)
            value = /*html*/`
                ${value}
                <details>
                    <summary>Show MathML</summary>
                    <xmp>${preview.innerHTML.trim()}</xmp>
                </details>
            `
        }
        preview.innerHTML = value 
    }
    /**
     * ## Component Connected
     * 
     * The methdod below is called when the component is added to the DOM. It
     * initializes the component and assigns classes to component root element
     * to make its styling easier.
     * 
     * Then we read the inital value from the corresponding attribute, if it 
     * exist, assign it to the input element and update the preview. Last, we
     * hook the input's `onchange` event to the `update` method.
     */
    protected connect() {
        this.body.innerHTML = this.content
        this.body.className = "body"
        this.body.append(this.input, this.preview)
        let initial = this.getAttribute("value")
        if (initial) {
            this.input.value = initial
            this.update()
        }
        this.input.oninput = () => this.update()
    }
    /**
     * ## Formatting MathML
     * 
     * The MathML produced by our converter does not contain any extra 
     * whitespace or formatting. This is intentional, to keep the result size
     * as small as possible. However, when presenting the MathML to the user,
     * it would be nice to have it correctly intended. This little helper copied
     * from Stack Overflow does that. It adds line breaks and spaces before 
     * elements depeneding on how deep they are in the DOM tree. So, the value
     * of the `innerHTML` property looks nicer when shown in the preview.
     */
    private format(node: Element, level: number): Element {
        let indentBefore = new Array(level++ + 1).join('  '),
            indentAfter  = new Array(level - 1).join('  '),
            textNode: Text
        for (var i = 0; i < node.children.length; i++) {
            textNode = document.createTextNode('\n' + indentBefore);
            node.insertBefore(textNode, node.children[i])
            this.format(node.children[i], level)
            if (node.lastElementChild == node.children[i]) {
                textNode = document.createTextNode('\n' + indentAfter)
                node.appendChild(textNode)
            }
        }
        return node
    }
}
/**
 * ## Defining Custom Element
 * 
 * The last thing to do is to register our component, and the custom element 
 * name that we give to it.
 */
customElements.define('asciimath-editor', AsciiMathEditor)