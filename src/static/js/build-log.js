/**
 *
 * @source:
 * https://git.savannah.gnu.org/cgit/guix/guix-cuirass.git/tree/src/static/js/build-log.js
 *
 * @licstart  The following is the entire license notice for the
 *  JavaScript code in this page.
 *
 * Copyright (C) 2023 Ludovic Courtès <ludo@gnu.org>
 * Copyright (C) 2024 Ricardo Wurmus <rekado@elephly.net>
 *
 *
 * The JavaScript code in this page is free software: you can
 * redistribute it and/or modify it under the terms of the GNU
 * General Public License (GNU GPL) as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option)
 * any later version.  The code is distributed WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU GPL for more details.
 *
 * As additional permission under GNU GPL version 3 section 7, you
 * may distribute non-source (e.g., minimized or compacted) forms of
 * that code without the copy of the GNU GPL normally required by
 * section 4, provided you include this license notice and a URL
 * through which recipients can access the Corresponding Source.
 *
 * @licend  The above is the entire license notice
 * for the JavaScript code in this page.
 *
 */

var ready = (callback) => {
  if (document.readyState != "loading") callback();
  else document.addEventListener("DOMContentLoaded", callback);
};
var generateElements = function (html) {
  const template = document.createElement('template');
  template.innerHTML = html.trim();
  return template.content.children;
};

ready(() => {
    const highlights = [
	{ regexp: new RegExp("^@ build-failed ([^ ]+) (.*)$"),
	  style: "text-danger"
        },
	{ regexp: new RegExp("^@ build-succeeded ([^ ]+) (.*)$"),
	  style: "text-success"
        },
        { regexp: new RegExp("^@ build-started (.*)$"),
          style: "text-info"
        },
        { regexp: new RegExp("^phase `(.*)' succeeded .*$"),
          style: "text-success"
        },
        { regexp: new RegExp("^phase `(.*)' failed .*$"),
          style: "text-danger"
        },
        { regexp: new RegExp("\\berror\\b"),
          style: "text-danger"
        },
        { regexp: new RegExp("\\bwarning\\b"),
          style: "text-warning"
        },
        { regexp: new RegExp("\\bPASS\\b"),
          style: "text-success"
        },
        { regexp: new RegExp("\\bFAIL\\b"),
          style: "text-danger"
        }
    ];

    const phaseStartRx = new RegExp("^starting phase [`'](.*)'$");
    const phaseEndRx = new RegExp("^phase [`'](.*)' (failed|succeeded) .*$");

    const logURL = document.URL + '/raw';

    async function readLog(response) {
        const decoder = new TextDecoder("utf-8");
        var body = "";

        for await (const chunk of response.body) {
            // FIXME: Should decode once we have the whole body as a single
            // Uint8Array.
            body += decoder.decode(chunk);
        }

	// Clear the build log area.
        var build_log = document.querySelector('#build-log');
	build_log.innerHTML = "";

	// Add a button to collapse/expand phases.
	var openness = true;
	const classOpened = "d-flex mb-3 lead text-info oi oi-fullscreen-exit";
	const classClosed = "d-flex mb-3 lead text-info oi oi-fullscreen-enter";
	const button = generateElements('<div id="phase-folding-button" title="Toggle phase visibility."></div>');
        button.className = classOpened;
        button.addEventListener('click', (e) => {
	    openness = !openness;
	    document.querySelectorAll("details").forEach((item) =>
		item.setAttribute('open', openness));
	    console.log("button", e.target);
	    e.target.setAttribute("class", openness ? classOpened : classClosed);
	});
        build_log.prepend(button);

	// Format the build log, line by line.
        const lines = body.split('\n');
	var parent = build_log;
        for (const line of lines) {
	    if (phaseStartRx.exec(line)) {
		// Open a <details> tag.
		const match = phaseStartRx.exec(line);
                var details = generateElements('<details open></details>'),
                    summary = generateElements('<summary class="text-info"></summary>');

                summary.textContent = match[1];
                details.append(summary);
                build_log.append(details);
		parent = details;
	    }

            var matched = false;
            for (const highlight of highlights) {
                const match = highlight.regexp.exec(line);
                if (match) {
                    parent.append(line.substring(0, match.index));
                    var span = generateElements('<span></span>');
                    span.className = highlight.style;
                    span.textContent = match[0];
                    parent.append(span);
                    parent.append(line.substring(match.index + match[0].length) + '\n');
		    matched = true;
		    break;
		}
	    }

	    if (!matched) {
		parent.append(line + '\n');
	    }

	    if (phaseEndRx.exec(line)) {
		// Close the <details> tag.
		parent = build_log;
	    }
	}
    }

    fetch(logURL)
	.then((response) => {
            const length = parseInt(response.headers.get("content-length"));
            console.log("build log size", length);

	    // If the log is too big (we're looking at the gzipped size here),
	    // redirect to the raw build log.
	    if (length > 2**20) {
		location.replace(logURL);
	    } else {
		readLog(response);
	    }
	});
});
