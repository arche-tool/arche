import './main.css';
import { Elm } from './Main.elm';
import ElmGoogleSignIn from './sign-in.js';

import logoPath from '../public/arche_logo.svg';
import iconPath from '../public/favicon.ico';
import manifest from '../public/manifest.json';

let googleSignOutComplete = new EventTarget();

let app = Elm.Main.init(
  {
    node: document.getElementById('root'),
    flags: {
      logo: logoPath,
      signOutComplete: googleSignOutComplete,
      oauth_azp: process.env.ELM_APP_OAUTH_AZP
    }
  }
);

app.ports.googleSignOut.subscribe(clientId => {
	ElmGoogleSignIn.signOut({
		port: app.ports.googleSignOutComplete,
		clientId: clientId,
	})
});