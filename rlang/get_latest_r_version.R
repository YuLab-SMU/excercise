#!/usr/bin/env Rscript
# =============================================================
# get_latest_r_version.R
#
# 从 CRAN 镜像探测各平台当前可下载的最新 R 版本，输出供 index.qmd
# 直接内联使用的下载地址（stdout，key=value 每行一个）。
#
# 为什么不是只取一个"最新版本号"再拼 URL？
#   R 4.6 起 macOS arm64 安装包移入新目录 sonoma-arm64（macOS 14+），
#   旧的 big-sur-arm64 目录停在 R 4.5.3；直接用版本号套旧模板会 404。
#   因此脚本按平台去镜像的目录页解析实际存在的安装文件：
#   - windows     : bin/windows/base/            -> R-X.Y.Z-win.exe
#   - macos-arm64 : bin/macosx/<os>-arm64/base/  -> R-X.Y.Z-arm64.pkg
#                   （自动发现所有 *-arm64 目录并取版本最高者，
#                     将来 CRAN 更换目录名也无需改本脚本）
#   - macos-intel : bin/macosx/<os>-x86_64/base/ -> R-X.Y.Z-x86_64.pkg
#
# 用法：
#   Rscript get_latest_r_version.R                 # 输出 version + 三行 URL
#   Rscript get_latest_r_version.R --version       # 只打印最新版本号
#   Rscript get_latest_r_version.R --mirror <url>  # 指定镜像（默认 hust）
#
# 网络失败时：仍输出内置"最近一次已知版本"的 URL（保证离线也能渲染），
# 同时把原因写入 stderr，供 index.qmd 显示提示。
# =============================================================

options(warn = 1, timeout = 30)  # timeout 只对新建连接生效，须在 url() 之前设置

default_mirror <- "https://mirrors.hust.edu.cn/CRAN"

## ---- 最近一次已知可用的版本（联网失败时的兜底，请随 R 发布手动更新） ----
fallback_version <- "4.6.1"
fallback_dir <- c(
    windows       = "/bin/windows/base/",
    `macos-arm64` = "/bin/macosx/sonoma-arm64/base/",
    `macos-intel` = "/bin/macosx/big-sur-x86_64/base/"
)
fallback_file <- c(
    windows       = paste0("R-", fallback_version, "-win.exe"),
    `macos-arm64` = paste0("R-", fallback_version, "-arm64.pkg"),
    `macos-intel` = paste0("R-", fallback_version, "-x86_64.pkg")
)

## ---- 命令行参数 ----
args <- commandArgs(trailingOnly = TRUE)
if (any(args %in% c("--help", "-h"))) {
    usage <- c(
        "get_latest_r_version.R -- 获取 CRAN 镜像上各平台最新 R 版本",
        "",
        "用法:",
        "  Rscript get_latest_r_version.R [--version] [--mirror <url>]",
        "",
        "  --version   只打印最新版本号（不打印 URL）",
        "  --mirror    指定 CRAN 镜像根 URL，默认 https://mirrors.hust.edu.cn/CRAN",
        "",
        "默认输出（stdout，key=value 每行一个）:",
        "  version=4.6.1",
        "  windows=<完整下载 URL>",
        "  macos-arm64=<完整下载 URL>",
        "  macos-intel=<完整下载 URL>",
        "  （若某平台探测失败会追加 warning=<原因>，URL 为该平台内置兜底版本）"
    )
    cat(usage, sep = "\n")
    quit(save = "no")
}

mirror <- default_mirror
if ("--mirror" %in% args) {
    i <- which(args == "--mirror")
    if (length(i) && i < length(args)) mirror <- sub("/+$", "", args[i + 1L])
}
version_only <- "--version" %in% args

## ---- 小工具 ----
get_page <- function(u) {
    con <- url(u)
    on.exit(close(con))
    paste(readLines(con, warn = FALSE), collapse = "\n")
}

## 从 html 中找出所有匹配 pattern 的串，去重
find_all <- function(html, pat) {
    unique(regmatches(html, gregexpr(pat, html, perl = TRUE))[[1]])
}

## 在文件名向量里挑版本号最高的安装文件，返回 list(file=, version=)
## suffix 形如 "win.exe" / "arm64.pkg" / "x86_64.pkg"
pick_latest <- function(files, suffix) {
    if (!length(files)) return(NULL)
    esc <- gsub(".", "\\.", suffix, fixed = TRUE)
    files <- files[grepl(sprintf("^R-[0-9]+\\.[0-9]+\\.[0-9]+-%s$", esc), files)]
    if (!length(files)) return(NULL)
    vers <- sub(sprintf("^R-(.*)-%s$", esc), "\\1", files)
    best <- which.max(numeric_version(vers))
    list(file = files[best], version = vers[best])
}

## 一个目录页里当前最新的安装文件名；取不到返回 NULL
latest_in_dir <- function(dir_url, suffix) {
    html <- get_page(dir_url)
    pick_latest(
        find_all(html, sprintf("R-[0-9]+\\.[0-9]+\\.[0-9]+-%s", suffix)),
        suffix
    )
}

## ---- 探测 ----
## 返回命名字符向量：version / windows / macos-arm64 / macos-intel
probe <- function() {
    problems <- character(0)
    report <- function(msg) problems <<- c(problems, msg)

    url <- list()   # 各平台完整下载 URL（成功探测或 fallback）
    ver <- list()   # 各平台成功探测到的版本

    ## -- Windows：bin/windows/base/ 页面只保留当前版本 ------------------
    win <- tryCatch(latest_in_dir(paste0(mirror, "/bin/windows/base/"), "win.exe"),
                    error = function(e) NULL, warning = function(w) NULL)
    if (is.null(win)) {
        report(sprintf("windows: cannot list %s/bin/windows/base/", mirror))
        url[["windows"]] <- paste0(mirror, fallback_dir[["windows"]], fallback_file[["windows"]])
    } else {
        url[["windows"]] <- paste0(mirror, "/bin/windows/base/", win$file)
        ver[["windows"]] <- win$version
    }

    ## -- macOS：从 bin/macosx/ 自动发现 <os>-arm64 / <os>-x86_64 目录 ----
    macos_top <- tryCatch(get_page(paste0(mirror, "/bin/macosx/")),
                          error = function(e) NULL, warning = function(w) NULL)
    if (is.null(macos_top)) {
        report(sprintf("macos: cannot list %s/bin/macosx/", mirror))
    }
    for (suffix in c("arm64", "x86_64")) {
        key <- if (suffix == "arm64") "macos-arm64" else "macos-intel"
        best <- NULL
        if (!is.null(macos_top)) {
            dirs <- sub(
                '^href="(.*)/"$', "\\1",
                find_all(macos_top, sprintf('href="[A-Za-z0-9._-]+-%s/"', suffix))
            )
            for (d in dirs) {
                got <- tryCatch(
                    latest_in_dir(paste0(mirror, "/bin/macosx/", d, "/base/"),
                                  paste0(suffix, ".pkg")),
                    error = function(e) NULL, warning = function(w) NULL
                )
                if (is.null(got)) next
                if (is.null(best) || numeric_version(got$version) > numeric_version(best$version)) {
                    best <- c(got, dir = d)
                }
            }
        }
        if (is.null(best)) {
            report(sprintf("%s: no current R build found on mirror", key))
            url[[key]] <- paste0(mirror, fallback_dir[[key]], fallback_file[[key]])
        } else {
            url[[key]] <- paste0(mirror, "/bin/macosx/", best$dir, "/base/", best$file)
            ver[[key]] <- best$version
        }
    }

    ## -- 版本号：取成功探测到的平台中的最高版本 -------------------
    latest_ver <- if (length(ver)) {
        as.character(max(numeric_version(unlist(ver))))
    } else {
        report(sprintf("cannot reach %s, using built-in fallback (R %s)",
                       mirror, fallback_version))
        fallback_version
    }

    if (length(problems)) {
        msg <- sprintf("WARNING: %s; using built-in fallback (R %s) where failed.",
                       paste(unique(problems), collapse = "; "), fallback_version)
        cat(msg, file = stderr())
    }

    out <- c(version = latest_ver, windows = url[["windows"]],
             `macos-arm64` = url[["macos-arm64"]], `macos-intel` = url[["macos-intel"]])
    if (length(problems)) {
        out <- c(out, warning = paste(unique(problems), collapse = "; "))
    }
    out
}

## ---- 主流程 ----
res <- probe()
if (version_only) {
    cat(res[["version"]], "\n")
} else {
    for (nm in names(res)) {
        cat(nm, "=", res[[nm]], "\n", sep = "")
    }
}
