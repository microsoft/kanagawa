# sample.callbacks [&equiv;](../index.md)

## Index

- <a href="../sample/callbacks.md" id="index-link"
  class="link">sample.callbacks</a>
  - <a href="../sample/callbacks.md#sample-callbacks-callback"
    id="index-link" class="link"><code>callback</code></a>
  - <a href="../sample/callbacks.md#sample-callbacks-callback_class"
    id="index-link" class="link"><code>callback_class</code></a>
  - <a href="../sample/callbacks.md#sample-callbacks-callback_class_2"
    id="index-link" class="link"><code>callback_class_2</code></a>
  - <a href="../sample/callbacks.md#sample-callbacks-callback_latency"
    id="index-link" class="link"><code>callback_latency</code></a>
  - <a href="../sample/callbacks.md#sample-callbacks-callback_test"
    id="index-link" class="link"><code>callback_test</code></a>

------------------------------------------------------------------------

<pre style="font-family:monospace;background-color:#f2f3f4;color:#373d3f;border:1px solid #dddddd;border-left:3px solid #0080ff;border-radius:5px 5px 5px 5px;margin-bottom:0.5em;padding:0.5em 1em 0.5em 1em">
<span style="color:#60ac39">using</span> <span style="font-weight:bold">callback</span> = (<span style="color:#60ac39">uint32</span>, <span style="color:#cfb017">bool</span>) -&gt; <span style="color:#60ac39">uint8</span> <a style="color:#c5c8c6" href="#sample-callbacks-callback" id="sample-callbacks-callback">§</a>
</pre>
<details>

<summary>Arguments</summary>

- <pre style="font-family:monospace;background-color:#f2f3f4;color:#373d3f;border:1px solid #dddddd;border-left:3px solid #0080ff;border-radius:5px 5px 5px 5px;margin-bottom:0.5em;padding:0.5em 1em 0.5em 1em">
  <span style="color:#60ac39">uint32</span>
  </pre>

  Pre-arg 1.

  Post-arg 1.

- <pre style="font-family:monospace;background-color:#f2f3f4;color:#373d3f;border:1px solid #dddddd;border-left:3px solid #0080ff;border-radius:5px 5px 5px 5px;margin-bottom:0.5em;padding:0.5em 1em 0.5em 1em">
  <span style="color:#cfb017">bool</span>
  </pre>

  Pre-arg 2.

  Post-arg 2.

</details>
<pre style="font-family:monospace;background-color:#f2f3f4;color:#373d3f;border:1px solid #dddddd;border-left:3px solid #0080ff;border-radius:5px 5px 5px 5px;margin-bottom:0.5em;padding:0.5em 1em 0.5em 1em">
<span style="color:#60ac39">class</span> <span style="font-weight:bold">callback_test</span> <a style="color:#c5c8c6" href="#sample-callbacks-callback_test" id="sample-callbacks-callback_test">§</a>
</pre>

Test callbacks

<details>

<summary>Callbacks and Fields</summary>

- <pre style="font-family:monospace;background-color:#f2f3f4;color:#373d3f;border:1px solid #dddddd;border-left:3px solid #0080ff;border-radius:5px 5px 5px 5px;margin-bottom:0.5em;padding:0.5em 1em 0.5em 1em">
  (<span style="color:#60ac39">uint32</span> <span style="font-weight:bold">x</span>) -&gt; <span style="color:#60ac39">uint2</span> <span style="font-weight:bold">callback</span> = <a style="color:#6684e1" href="../sample/callbacks.md#sample-callbacks-callback_test"><span style="font-weight:bold">callback_test</span></a>::<span style="font-weight:bold">default_callback</span> <a style="color:#c5c8c6" href="#sample-callbacks-callback_test-callback" id="sample-callbacks-callback_test-callback">§</a>
  </pre>

</details>
<pre style="font-family:monospace;background-color:#f2f3f4;color:#373d3f;border:1px solid #dddddd;border-left:3px solid #0080ff;border-radius:5px 5px 5px 5px;margin-bottom:0.5em;padding:0.5em 1em 0.5em 1em">
<span style="color:#60ac39">class</span> <span style="font-weight:bold">callback_class</span> <a style="color:#c5c8c6" href="#sample-callbacks-callback_class" id="sample-callbacks-callback_class">§</a>
</pre>
<details>

<summary>Aliases</summary>

- <pre style="font-family:monospace;background-color:#f2f3f4;color:#373d3f;border:1px solid #dddddd;border-left:3px solid #0080ff;border-radius:5px 5px 5px 5px;margin-bottom:0.5em;padding:0.5em 1em 0.5em 1em">
  <span style="color:#60ac39">using</span> <span style="font-weight:bold">doc_callback_t</span> = () -&gt; <span style="color:#60ac39">void</span> <a style="color:#c5c8c6" href="#sample-callbacks-callback_class-doc_callback_t" id="sample-callbacks-callback_class-doc_callback_t">§</a>
  </pre>

  doc callback type

</details>
<details>

<summary>Callbacks and Fields</summary>

- <pre style="font-family:monospace;background-color:#f2f3f4;color:#373d3f;border:1px solid #dddddd;border-left:3px solid #0080ff;border-radius:5px 5px 5px 5px;margin-bottom:0.5em;padding:0.5em 1em 0.5em 1em">
  <a style="color:#6684e1" href="../sample/callbacks.md#sample-callbacks-callback_class"><span style="font-weight:bold">callback_class</span></a>::<a style="color:#6684e1" href="../sample/callbacks.md#sample-callbacks-callback_class-doc_callback_t"><span style="font-weight:bold">doc_callback_t</span></a> <span style="font-weight:bold">callback_before</span> = <a style="color:#6684e1" href="../sample/callbacks.md#sample-callbacks-callback_class-default_callback"><span style="font-weight:bold">default_callback</span></a> <a style="color:#c5c8c6" href="#sample-callbacks-callback_class-callback_before" id="sample-callbacks-callback_class-callback_before">§</a>
  </pre>

  callback with default before decl

- <pre style="font-family:monospace;background-color:#f2f3f4;color:#373d3f;border:1px solid #dddddd;border-left:3px solid #0080ff;border-radius:5px 5px 5px 5px;margin-bottom:0.5em;padding:0.5em 1em 0.5em 1em">
  <a style="color:#6684e1" href="../sample/callbacks.md#sample-callbacks-callback_class"><span style="font-weight:bold">callback_class</span></a>::<a style="color:#6684e1" href="../sample/callbacks.md#sample-callbacks-callback_class-doc_callback_t"><span style="font-weight:bold">doc_callback_t</span></a> <span style="font-weight:bold">callback</span> = <a style="color:#6684e1" href="../sample/callbacks.md#sample-callbacks-callback_class"><span style="font-weight:bold">callback_class</span></a>::<a style="color:#6684e1" href="../sample/callbacks.md#sample-callbacks-callback_class-default_callback"><span style="font-weight:bold">default_callback</span></a> <a style="color:#c5c8c6" href="#sample-callbacks-callback_class-callback" id="sample-callbacks-callback_class-callback">§</a>
  </pre>

  callback

- <pre style="font-family:monospace;background-color:#f2f3f4;color:#373d3f;border:1px solid #dddddd;border-left:3px solid #0080ff;border-radius:5px 5px 5px 5px;margin-bottom:0.5em;padding:0.5em 1em 0.5em 1em">
  (<span style="color:#60ac39">uint32</span> <span style="font-weight:bold">x</span>) -&gt; <span style="color:#60ac39">uint2</span> <span style="font-weight:bold">callback1</span> <a style="color:#c5c8c6" href="#sample-callbacks-callback_class-callback1" id="sample-callbacks-callback_class-callback1">§</a>
  </pre>

</details>
<details>

<summary>Methods</summary>

- <pre style="font-family:monospace;background-color:#f2f3f4;color:#373d3f;border:1px solid #dddddd;border-left:3px solid #0080ff;border-radius:5px 5px 5px 5px;margin-bottom:0.5em;padding:0.5em 1em 0.5em 1em">
  <span style="color:#60ac39">void</span> <span style="font-weight:bold">default_callback</span>() <a style="color:#c5c8c6" href="#sample-callbacks-callback_class-default_callback" id="sample-callbacks-callback_class-default_callback">§</a>
  </pre>

  doc callback default

</details>
<pre style="font-family:monospace;background-color:#f2f3f4;color:#373d3f;border:1px solid #dddddd;border-left:3px solid #0080ff;border-radius:5px 5px 5px 5px;margin-bottom:0.5em;padding:0.5em 1em 0.5em 1em">
<span style="color:#60ac39">class</span> <span style="font-weight:bold">callback_class_2</span> <a style="color:#c5c8c6" href="#sample-callbacks-callback_class_2" id="sample-callbacks-callback_class_2">§</a>
</pre>
<details>

<summary>Types</summary>

- <pre style="font-family:monospace;background-color:#f2f3f4;color:#373d3f;border:1px solid #dddddd;border-left:3px solid #0080ff;border-radius:5px 5px 5px 5px;margin-bottom:0.5em;padding:0.5em 1em 0.5em 1em">
  <span style="color:#60ac39">class</span> <span style="font-weight:bold">inner_callback_class</span> <a style="color:#c5c8c6" href="#sample-callbacks-callback_class_2-inner_callback_class" id="sample-callbacks-callback_class_2-inner_callback_class">§</a>
  </pre>
  <details>

  <summary>Callbacks and Fields</summary>

  - <pre style="font-family:monospace;background-color:#f2f3f4;color:#373d3f;border:1px solid #dddddd;border-left:3px solid #0080ff;border-radius:5px 5px 5px 5px;margin-bottom:0.5em;padding:0.5em 1em 0.5em 1em">
    <a style="color:#6684e1" href="../sample/callbacks.md#sample-callbacks-callback_class"><span style="font-weight:bold">callback_class</span></a>::<a style="color:#6684e1" href="../sample/callbacks.md#sample-callbacks-callback_class-doc_callback_t"><span style="font-weight:bold">doc_callback_t</span></a> <span style="font-weight:bold">inner_pub_callback</span> = <a style="color:#6684e1" href="../sample/callbacks.md#sample-callbacks-callback_class"><span style="font-weight:bold">callback_class</span></a>::<a style="color:#6684e1" href="../sample/callbacks.md#sample-callbacks-callback_class-default_callback"><span style="font-weight:bold">default_callback</span></a> <a style="color:#c5c8c6" href="#sample-callbacks-callback_class_2-inner_callback_class-inner_pub_callback" id="sample-callbacks-callback_class_2-inner_callback_class-inner_pub_callback">§</a>
    </pre>

  - <pre style="font-family:monospace;background-color:#f2f3f4;color:#373d3f;border:1px solid #dddddd;border-left:3px solid #0080ff;border-radius:5px 5px 5px 5px;margin-bottom:0.5em;padding:0.5em 1em 0.5em 1em">
    <a style="color:#6684e1" href="../sample/callbacks.md#sample-callbacks-callback_class"><span style="font-weight:bold">callback_class</span></a>::<a style="color:#6684e1" href="../sample/callbacks.md#sample-callbacks-callback_class-doc_callback_t"><span style="font-weight:bold">doc_callback_t</span></a> <span style="font-weight:bold">inner_pub_callback2</span> = <a style="color:#6684e1" href="../sample/callbacks.md#sample-callbacks-callback_class_2"><span style="font-weight:bold">callback_class_2</span></a>::<span style="font-weight:bold">priv_callback</span> <a style="color:#c5c8c6" href="#sample-callbacks-callback_class_2-inner_callback_class-inner_pub_callback2" id="sample-callbacks-callback_class_2-inner_callback_class-inner_pub_callback2">§</a>
    </pre>

  - <pre style="font-family:monospace;background-color:#f2f3f4;color:#373d3f;border:1px solid #dddddd;border-left:3px solid #0080ff;border-radius:5px 5px 5px 5px;margin-bottom:0.5em;padding:0.5em 1em 0.5em 1em">
    <a style="color:#6684e1" href="../sample/callbacks.md#sample-callbacks-callback_class"><span style="font-weight:bold">callback_class</span></a>::<a style="color:#6684e1" href="../sample/callbacks.md#sample-callbacks-callback_class-doc_callback_t"><span style="font-weight:bold">doc_callback_t</span></a> <span style="font-weight:bold">inner_pub_callback3</span> = <a style="color:#6684e1" href="../sample/callbacks.md#sample-callbacks-callback_class_2"><span style="font-weight:bold">callback_class_2</span></a>::<a style="color:#6684e1" href="../sample/callbacks.md#sample-callbacks-callback_class_2-pub_callback"><span style="font-weight:bold">pub_callback</span></a> <a style="color:#c5c8c6" href="#sample-callbacks-callback_class_2-inner_callback_class-inner_pub_callback3" id="sample-callbacks-callback_class_2-inner_callback_class-inner_pub_callback3">§</a>
    </pre>

  </details>

</details>
<details>

<summary>Callbacks and Fields</summary>

- <pre style="font-family:monospace;background-color:#f2f3f4;color:#373d3f;border:1px solid #dddddd;border-left:3px solid #0080ff;border-radius:5px 5px 5px 5px;margin-bottom:0.5em;padding:0.5em 1em 0.5em 1em">
  <a style="color:#6684e1" href="../sample/callbacks.md#sample-callbacks-callback_class"><span style="font-weight:bold">callback_class</span></a>::<a style="color:#6684e1" href="../sample/callbacks.md#sample-callbacks-callback_class-doc_callback_t"><span style="font-weight:bold">doc_callback_t</span></a> <span style="font-weight:bold">priv_callback2</span> = <a style="color:#6684e1" href="../sample/callbacks.md#sample-callbacks-callback_class"><span style="font-weight:bold">callback_class</span></a>::<a style="color:#6684e1" href="../sample/callbacks.md#sample-callbacks-callback_class-default_callback"><span style="font-weight:bold">default_callback</span></a> <a style="color:#c5c8c6" href="#sample-callbacks-callback_class_2-priv_callback2" id="sample-callbacks-callback_class_2-priv_callback2">§</a>
  </pre>

  private callback 2

- <pre style="font-family:monospace;background-color:#f2f3f4;color:#373d3f;border:1px solid #dddddd;border-left:3px solid #0080ff;border-radius:5px 5px 5px 5px;margin-bottom:0.5em;padding:0.5em 1em 0.5em 1em">
  <a style="color:#6684e1" href="../sample/callbacks.md#sample-callbacks-callback_class"><span style="font-weight:bold">callback_class</span></a>::<a style="color:#6684e1" href="../sample/callbacks.md#sample-callbacks-callback_class-doc_callback_t"><span style="font-weight:bold">doc_callback_t</span></a> <span style="font-weight:bold">pub_callback</span> = <a style="color:#6684e1" href="../sample/callbacks.md#sample-callbacks-callback_class"><span style="font-weight:bold">callback_class</span></a>::<a style="color:#6684e1" href="../sample/callbacks.md#sample-callbacks-callback_class-default_callback"><span style="font-weight:bold">default_callback</span></a> <a style="color:#c5c8c6" href="#sample-callbacks-callback_class_2-pub_callback" id="sample-callbacks-callback_class_2-pub_callback">§</a>
  </pre>

  public callback

</details>
<pre style="font-family:monospace;background-color:#f2f3f4;color:#373d3f;border:1px solid #dddddd;border-left:3px solid #0080ff;border-radius:5px 5px 5px 5px;margin-bottom:0.5em;padding:0.5em 1em 0.5em 1em">
<span style="color:#60ac39">class</span> <span style="font-weight:bold">callback_latency</span> <a style="color:#c5c8c6" href="#sample-callbacks-callback_latency" id="sample-callbacks-callback_latency">§</a>
</pre>

Latency and callback

<details>

<summary>Callbacks and Fields</summary>

- <pre style="font-family:monospace;background-color:#f2f3f4;color:#373d3f;border:1px solid #dddddd;border-left:3px solid #0080ff;border-radius:5px 5px 5px 5px;margin-bottom:0.5em;padding:0.5em 1em 0.5em 1em">
  [[<span style="color:#ffa500">latency</span>(<span style="color:#d73737">2</span>)]] (<span style="color:#60ac39">uint32</span> <span style="font-weight:bold">x</span>) -&gt; <span style="color:#60ac39">uint2</span> <span style="font-weight:bold">lc_callback</span> <a style="color:#c5c8c6" href="#sample-callbacks-callback_latency-lc_callback" id="sample-callbacks-callback_latency-lc_callback">§</a>
  </pre>

</details>
