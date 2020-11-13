exports.createAudio = ({ url, loop }) => {
    const audio = new Audio(url)
    if (loop) {
        audio.setAttribute("loop", "loop")
    }

    return audio
}

exports.playAudio = (audio) => () => {
    audio.play()
}