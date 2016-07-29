var websocket;
      function init() {
              connect();
      };

      function connect()
      {
          websocket = new WebSocket('ws://127.0.0.1:8888/websocket');
      };
      
      function disconnect() {
          websocket.close();
      }; 

      function toggle_connection(){
          if(websocket.readyState == websocket.OPEN){
              disconnect();
          } else {
              connect();
          };
      };

      function sendTxt() {
          if(websocket.readyState == websocket.OPEN){
              websocket.send(txt);
          }
      };
