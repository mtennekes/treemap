tmSetHCLoptions <- function(palette.HCL.options) {
    # palette.HCL.options
    palette.HCL.options.temp <- list(hue_start=30, hue_end=390, hue_perm=TRUE, hue_rev=TRUE, hue_fraction=0.5, chroma=60, luminance=70, chroma_slope=5, luminance_slope=-10)
    if (!missing(palette.HCL.options)) if(!is.null(palette.HCL.options)) {
        if (!is.list(palette.HCL.options) | !all(names(palette.HCL.options)%in%names(palette.HCL.options.temp))) stop("Incorrect palette.HCL.options")
        palette.HCL.options.temp[names(palette.HCL.options)] <- palette.HCL.options
    }
    palette.HCL.options.temp
}