exports.imageSize = (img) => ({ w: img.naturalWidth, h: img.naturalHeight })

exports.setSmoothingEnabled = ctx => enabled => () => {
    return ctx.imageSmoothingEnabled = enabled
}