---
layout: doc-page
title: "Site versioning"
---

Scaladoc provides a convenient way to switch between different versions of the documentation. The feature is useful if we want to expose older docs for users that didn't migrate to the new version of our library.

### How to setup it

The feature was designed for easy scalability with no need to regenerate all scaladocs after adding a new version. To do so a new setting is introduced:  `-versions-dictionary-url`. Its argument must be a URL to a JSON document holding information about the locations of specific versions. The JSON file has single property `versions` that holds the dictionary associating the labels of specific versions of the documentation to the URLs pointing to their index.html

Example JSON file:
```
{
  "versions": {
    "3.0.x": "https://dotty.epfl.ch/3.0.x/docs/index.html",
    "Nightly": "https://dotty.epfl.ch/docs/index.html"
  }
}
```

This enforce us to provide the setting while generating docs for each of the versions, however it gives us more flexibility later. If you want to add a version of the API docs next to the previous 5 versions that you have already published, then you only need to upload the new docs to a web server and add a new entry to the JSON file. All versions of the site will now become aware of the new site version.

The important thing to note is that there is only one JSON file to avoid redundancy and each scaladoc must set up its URL location beforehand, for example, in sbt:

```
doc / scalacOptions ++= Seq("-versions-dictionary-url", "https://dotty.epfl.ch/versions.json")
```


### How does it look from user perspective

Providing a JSON file via `-versions-dictionary-url` enables scaladoc to link between versions. It is also convenient to be able to change the revision label in the drop-down menu. Everything will change automatically.

![]({{ site.baseurl }}images/scaladoc/nightly.gif)
