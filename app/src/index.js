import './main.css';
import { Elm } from './Main.elm';

import logoPath from '../public/arche_logo.svg';
import iconPath from '../public/favicon.ico';
import manifest from '../public/manifest.json';

Elm.Main.init({
  node: document.getElementById('root'),
  flags: {logo: logoPath}
});
