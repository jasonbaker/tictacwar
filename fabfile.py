from fabric.api import *
from fabric.contrib.project import rsync_project

env.hosts = ['jason-baker.com']

def upload_static():
    rsync_project(local_dir='static',
                  remote_dir='/var/www',
                  delete=False)

def compile_code():
    local("raco make rack-serve.rkt")

def upload_code():
    rsync_project(local_dir='.',
                  remote_dir='/opt/tictacwar',
                  exclude=['static', '*~', '.git', 'fabfile.py*'])

def upload_supervisor():
    put('supervisor/tictacwar.conf', '/etc/supervisor/conf.d/')

def start_server():
    sudo('/etc/init.d/supervisor stop')
    sudo('/etc/init.d/supervisor start')

def deploy():
    """Upload the static files, code, supervisor config, and restart supervisor"""
    upload_static()
    compile_code()
    upload_code()
    upload_supervisor()
    start_server()
