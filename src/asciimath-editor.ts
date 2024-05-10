import * as ce from 'litscript/src/custom-elem'
import { asciiToMathML } from '.'

import "./asciimath-editor.css"

export class AsciiMathEditor extends ce.StyledElement {
    private label = /*html*/`
            <span class="label">Preview</span>
    `
    private content = /*html*/`
        <textarea name="input" id="input" rows="5" cols="40" spellcheck="false"
            placeholder="Enter AsciiMath Equation">
        </textarea>
        <div id="preview">
            ${this.label}
        </div>
    `
    constructor() {
        super("asciimath-editor")
    }

    private get input(): HTMLTextAreaElement {
        return this.shadowRoot!.getElementById("input") as HTMLTextAreaElement
    }

    private get preview(): HTMLDivElement {
        return this.shadowRoot!.getElementById("preview") as HTMLDivElement
    }

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

    protected connect() {
        this.body.innerHTML = this.content
        this.body.className = "body"
        this.body.append(this.input, this.preview)
        let initial = this.getAttribute("value")
        if (initial) {
            this.input.value = initial
            this.update()
        }
        this.input.onchange = () => this.update()
    }

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

customElements.define('asciimath-editor', AsciiMathEditor)