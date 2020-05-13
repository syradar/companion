import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

/**
 * UUIDGeneratorBrowser : () => string
 */
const UUIDGeneratorBrowser = () =>
  ([1e7] + -1e3 + -4e3 + -8e3 + -1e11).replace(/[018]/g, (c) =>
    (
      c ^
      (crypto.getRandomValues(new Uint8Array(1))[0] & (15 >> (c / 4)))
    ).toString(16)
  );

const app = Elm.Main.init({
  node: document.getElementById('root'),
});

app.ports.getUUID.subscribe(() => {
  app.ports.recieveUUID.send(UUIDGeneratorBrowser());
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
