;;; init.el --- -*- lexical-binding: t; -*-
;;(setq debug-on-error t)

(defvar hikizan-dir (file-name-directory load-file-name))
(defvar hikizan-lisp-dir (expand-file-name "lisp" hikizan-dir))

(add-to-list 'load-path hikizan-lisp-dir)

(require 'hikizan-package-manager)
(require 'hikizan-util)
(require 'hikizan-completion)
(require 'hikizan-project)
(require 'hikizan-editor)
(require 'hikizan-org)
(require 'hikizan-snippet)
(require 'hikizan-programming)
(require 'hikizan-llm)
(require 'hikizan-ui)
(require 'hikizan-keybinds)

(setq system-time-local "C") ;; to avoid Japanese in the time stamp
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("915207fe272dc6f7cb40b10c06a6bf2092742145df18c73fe449573621e04a4f" "426a9b0e9aa78abb0b23b1b69c47f5086cadea443791cd28d8a09b7e7c301e67" "3942a866e2b4a5a22ac374cd7e9cc40984f81b214401ee80189ee717da1952ea" "602d30251f3124c08a4142073f13cb2477c3f99a255269a4b0cf20f812b1cfb4" "beedef2572ea08beb4c83c86b3960c23791e18b3c35dc55fb10aebbc5abac65f" "1e861964006d6310dacdcffc29875f79e70825392b58285a414f6e123b72cec3" "94b5b96f918a0ba998a3bbbc949df98af96a00b208eb00c7b412a9d5cf57cfb5" "8b4200a3dfb6c9af54acd39acc2ea9c23c3c1665eb1bbb4bb6d2fc0cbd7ec06d" "370f0b4ee3da141e74444ff162a71c08fe51090a1c986aabd1950be2766dd0e5" "aaa68cf579a1ff9cb1c42c94eb7fad29574cbd85e2cb16174c73afc68fed03fd" "3d4d41a69a165e0629bc38b42f480ffa46d5462c262e7b2e1540aba7c578a650" "4c385bfc4c13851b71b9c88388a10e9e62d4753f4a2fc76594d59d1ff1bb1577" "dc01a29db7803876c5a327c1b0d445bf9580a8bbf0515b630cc6bdd77aa38e81" "dd8c83478ce607c02945e7fe42e5c534634624a1296591c6e8e310b9a8d6eb38" "9fe340349d9582e1faa9b92d3384a171ffb1f68443e403ac8759a78bd1abe4a7" "bd5345f2005b2093f9792c8e157509383f045ea4c2efd3dedbc4c192e363c60c" "1ada7c5a66b72c1b0b0d2b4b35080a433f9b00fc60adb8758f84028fae48a72a" "046dc446f1229c3192e03084667ef1e755342be4c2aa40c982d2587e8a38fb04" "f934852a61696b806c1a29faf2c3f28ff3f89e748d36e07854dedf3071a7aeb5" "8992dee31510988c7f56a15a839134aa1954ec8a42be5ca8eb6369eb3fd109fc" "de46fb3a2501a4930264f53706cd96db24d3c73e601f537a45bb34f49e3270ae" "c0810c5cd2a6b072e8017e46e5d52cc79a84a094f4b45a6db57260f1fb25838c" "49b84f4468f42609ebc0f33414763cfff3717a4489e4064f7baa859fb6bc0f64" "39877aa5ba4aa3f6374bc199f275a70b6a615a7ee91cbdeed717be94cacf4201" "60edaa7d2b5f20f6a603ce78ea39b17b1cf5763d70dc7098b9dadd791896f4a0" "26d3a734ae1d24df03519068eb72997f790c0a341816de8797d23f34a16db35f" "ca87fe9219804fdb6b90ca17e818c246c1b6f96f43d193073493c2f23dbefdc4" "d4352d0d2466c4c03f1fdedfe30a6e57d1bbb99c9082185ad0a7a4279261fd68" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
