def usePreviewFeatureTransitively = usePreviewFeature
def usePreviewFeatureDirectly = previewFeature // error

@scala.annotation.preview 
def wrappedPreviewFeature = previewFeature