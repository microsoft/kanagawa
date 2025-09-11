# sample.examples [&equiv;](../index.md)

## Index

- <a href="../sample/examples.md" id="index-link"
  class="link">sample.examples</a>
  - <a href="../sample/examples.md#sample-examples-collapse_example"
    id="index-link" class="link"><code>collapse_example</code></a>
  - <a href="../sample/examples.md#sample-examples-collapse_examples"
    id="index-link" class="link"><code>collapse_examples</code></a>
  - <a href="../sample/examples.md#sample-examples-multiple_examples"
    id="index-link" class="link"><code>multiple_examples</code></a>
  - <a href="../sample/examples.md#sample-examples-nested_example"
    id="index-link" class="link"><code>nested_example</code></a>
  - <a href="../sample/examples.md#sample-examples-summary_examples"
    id="index-link" class="link"><code>summary_examples</code></a>

------------------------------------------------------------------------

<pre style="font-family:monospace;background-color:#f2f3f4;color:#373d3f;border:1px solid #dddddd;border-left:3px solid #0080ff;border-radius:5px 5px 5px 5px;margin-bottom:0.5em;padding:0.5em 1em 0.5em 1em">
<span style="color:#cfb017">string</span> <span style="font-weight:bold">collapse_example</span>() <a style="color:#c5c8c6" href="#sample-examples-collapse_example" id="sample-examples-collapse_example">§</a>
</pre>

This is a comment

<details>
<summary>Example</summary>

``` kanagawa
>>> collapse_example()
"hello"
```

</details>
<pre style="font-family:monospace;background-color:#f2f3f4;color:#373d3f;border:1px solid #dddddd;border-left:3px solid #0080ff;border-radius:5px 5px 5px 5px;margin-bottom:0.5em;padding:0.5em 1em 0.5em 1em">
<span style="color:#60ac39">void</span> <span style="font-weight:bold">collapse_examples</span>() <a style="color:#c5c8c6" href="#sample-examples-collapse_examples" id="sample-examples-collapse_examples">§</a>
</pre>
<details>
<summary>Examples</summary>

``` kanagawa
>>> collapse_examples()
"hello"

>>> collapse_examples()
"world"
```

</details>
<pre style="font-family:monospace;background-color:#f2f3f4;color:#373d3f;border:1px solid #dddddd;border-left:3px solid #0080ff;border-radius:5px 5px 5px 5px;margin-bottom:0.5em;padding:0.5em 1em 0.5em 1em">
<span style="color:#60ac39">void</span> <span style="font-weight:bold">summary_examples</span>() <a style="color:#c5c8c6" href="#sample-examples-summary_examples" id="sample-examples-summary_examples">§</a>
</pre>

Test optional preceding summary.

<details>
<summary>Examples</summary>

``` kanagawa
>>> shift_array_left<4>({0, 1, 2, 3, 4}, 10);
{0, 0, 0, 0}
```

Shift 5 left by 0:

``` kanagawa
>>> shift_array_left<5>({0, 1, 2, 3, 4}, 0);
{0, 1, 2, 3, 4}
```

Shift 5 left by 2 and return 4:

``` kanagawa
>>> shift_array_left<4>({0, 1, 2, 3, 4}, 2);
{0, 0, 0, 1}
```

</details>
<pre style="font-family:monospace;background-color:#f2f3f4;color:#373d3f;border:1px solid #dddddd;border-left:3px solid #0080ff;border-radius:5px 5px 5px 5px;margin-bottom:0.5em;padding:0.5em 1em 0.5em 1em">
<span style="color:#60ac39">class</span> <span style="font-weight:bold">nested_example</span> <a style="color:#c5c8c6" href="#sample-examples-nested_example" id="sample-examples-nested_example">§</a>
</pre>

Test nested examples

<details>
<summary>Example</summary>

``` kanagawa
>>> some_example()
output
```

</details>
<details>

<summary>Callbacks and Fields</summary>

- <pre style="font-family:monospace;background-color:#f2f3f4;color:#373d3f;border:1px solid #dddddd;border-left:3px solid #0080ff;border-radius:5px 5px 5px 5px;margin-bottom:0.5em;padding:0.5em 1em 0.5em 1em">
  <span style="color:#60ac39">const</span> <span style="color:#60ac39">auto</span> <span style="font-weight:bold">variable</span> <a style="color:#c5c8c6" href="#sample-examples-nested_example-variable" id="sample-examples-nested_example-variable">§</a>
  </pre>
  <details>
  <summary>Example</summary>

  ``` kanagawa
  >>> some_nested_example()
  output
  ```

  </details>

</details>
<pre style="font-family:monospace;background-color:#f2f3f4;color:#373d3f;border:1px solid #dddddd;border-left:3px solid #0080ff;border-radius:5px 5px 5px 5px;margin-bottom:0.5em;padding:0.5em 1em 0.5em 1em">
<span style="color:#60ac39">void</span> <span style="font-weight:bold">multiple_examples</span>() <a style="color:#c5c8c6" href="#sample-examples-multiple_examples" id="sample-examples-multiple_examples">§</a>
</pre>

Test multiple examples

<details>
<summary>Example</summary>

``` kanagawa
>>> one_example()
1
```

</details>
<details>
<summary>Example</summary>

``` kanagawa
>>> two_example()
2
```

</details>
<details>
<summary>Example</summary>

``` kanagawa
>>> three_example()
3
```

</details>

A helpful paragraph between examples.

<details>
<summary>Example</summary>

``` kanagawa
>>> four_example()
4
```

</details>
